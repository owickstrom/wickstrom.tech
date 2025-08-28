---
title: "Finding Bugs in a Coding Agent with Lightweight DST" 
date: "August 28, 2025"
author: "Oskar Wickström"
---

[Amp](https://ampcode.com/) is a coding agent which I've been working on the
last six months at Sourcegraph. And in the last couple of weeks, I've been
building on a testing rig inspired by [Deterministic Simulation
Testing](https://github.com/ivanyu/awesome-deterministic-simulation-testing)
(DST) to test the most crucial parts of the system. DST is closely related to
fuzzing and property-based testing.

The goal is to get one of Amp's most central pieces, the _ThreadWorker_, under
heavy scrutiny. We've had a few perplexing bug reports, where users experienced
corrupted [threads](https://ampcode.com/manual#threads), LLM API errors from
invalid tool calls, and more vague issues like "it seems like it's spinning
forever." Reproducing such problems manually is usually somewhere between
impractical and impossible. I want to reproduce them deterministically, and in
a way where we can debug and fix them. And beyond the known ones, I'd like to
find the currently unknown ones before our users hit them.

Generative testing to the rescue!

## Approach: Lightweight DST in TypeScript

Amp is written in TypeScript, which is an ecosystem currently not drowning in
fuzzing tools. My starting point was using
[`jsfuzz`](https://www.npmjs.com/package/jsfuzz), which I hadn't used before
but it looked promising. However, I had a bunch of problems getting it to run
together with our Bun stack. One could use fast-check, but as far as I can
tell, the model-based testing they support doesn't fit with our needs. We don't
have a model of the system, and we need to generate values in multiple places
as the test runs. So, I decided to build something from scratch for our
purposes.

I borrowed an idea I got from
[matklad](https://tigerbeetle.com/blog/2023-03-28-random-fuzzy-thoughts/) last
year: instead of passing a seeded PRNG to generate test input, we generate an
entropy [`Buffer`](https://bun.com/docs/api/binary-data#buffer) with random
contents, and track our position in that array with a cursor. Drawing a random
byte _consumes_ the byte at the current position and increments the cursor. We
don't know up-front how many bytes we need for a given fuzzer, so the entropy
buffer grows dynamically when needed, appending more random bytes. This,
together with a bunch of methods for drawing different types of values, is
packaged up in an `Entropy` class:

```typescript
class Entropy {
  random(count): UInt8Array { ... }
  randomRange(minIncl: number, maxExcl: number): number { ... }
  // ... lots of other stuff
}
```

A fuzzer is an ES module written in TypeScript, exporting a single function:

```typescript
export async function fuzz(entropy: Entropy) {
  // test logic here
}
```

Any exception thrown by `fuzz` is considered a test failure. We use the
`node:assert` module for our test assertions, but it could be anything.

Another program, the fuzz runner, imports a built fuzzer module and runs as
many tests it can before a given timeout. If it finds a failure, it prints out
the command to reproduce that failure:

```
Fuzzing example.fuzzer.js iteration 1000...
Fuzzing example.fuzzer.js iteration 2000...

Fuzzer failed: AssertionError [ERR_ASSERTION]: 3 != 4
  at [...]

Reproduce with:

  bun --console-depth=10 scripts/fuzz.ts \
    dist/example.fuzzer.js \
    --verbose \
    --reproduce=1493a513f88d0fd9325534c33f774831
```

Why use this `Entropy` rather than a seed? More about that at the end of the
post!

## The ThreadWorker Fuzzer

In the fuzzer for our `ThreadWorker`, we stub out all IO and other
nondeterministic components, and we install [fake
timers](https://sinonjs.org/releases/latest/fake-timers/) to control when and
how asynchronous code is run. In effect, we have _determinism_ and _simulation_
to run tests in, so I guess it qualifies as DST.

The test simulates a sequence of user actions (send message, cancel, resume,
and wait). Similarly, it simulates responses from tool calls (like the agent
reading a file) and from inference backends (like the Anthropic API). We inject
faults and delays in both tool calls and inference requests to test our error
handling and possible race conditions.

After all user actions have been executed, we make sure to approve any pending
tool calls that require confirmation. Next, we tell the fake timer to run all
outstanding timers until the queue is empty; like fast-forwarding until there's
nothing left to do. Finally, we check that the thread is _idle_, i.e. that
there's no ongoing inference and that all tool calls have terminated. This is
a liveness property.

After the liveness property, we check a bunch of safety properties:

* all messages posted by the user are present in the thread
* all message pairs involving tools calls are valid according to Anthropic's API specification
* all tool calls have settled in expected terminal states

Some of these are targeted at specific known bugs, while some are more general
but have found bugs we did not expect. 

Here's a highly simplified version of the fuzzer:

```typescript
export async function fuzz(entropy: Entropy) {
  const clock = sinon.useFakeTimers({
    loopLimit: 1_000_000,
  })
  const worker = setup() // including stubbing IO, etc

  try {
    const resumed = worker.resume()
    await clock.runAllAsync()
    await resumed

    const actions: UserAction[] = []
    async function run() {
      for (let round = 0; round < entropy.randomRange(1, 50); round++) {
        const action = await generateNextAction(entropy, worker)
        switch (action.type) {
          case 'user-message':
            await worker.handle({
            ...action,
            type: 'user:message',
          })
          break
          case 'cancel':
            await worker.cancel()
          break
          case 'resume':
            await worker.resume()
          break
          case 'sleep':
            await sleep(action.milliseconds)
          break
          case 'approve': {
            await approveTool(action.threadID, action.toolUseID)
            break
          }
        }
      }

      // Approve any remaining tool uses to ensure termination into an 
      // idle thread state
      const blockedTools = await blockedToolUses()
      await Promise.all(blockedTools.map(approve))
    }

    const done = run()
    await clock.runAllAsync()
    await done

    // check liveness and safety properties
    // ...
  } finally {
    sinon.restore()
  }
}
```

Now, let's dig into the findings!

## Results

Given I've been working on this for about a week in total, I'm very happy with
the outcome. Here are some issues the fuzzer found:

**Corrupted thread due to eagerly starting tool calls during streaming**

: While streaming tool use blocks from the Anthropic API, we invoked tools
    eagerly, while not all of them were finished streaming. This, in combination
    with how state was managed, led to tool results being incorrectly split across
    messages. Anthropic's API would reject any further requests, and the thread
    would essentially be corrupted. This was reported by a user and was the first
    issue we found and fixed using the fuzzer.

    Another variation, which the fuzzer also found, this was a race condition
    where user messages interfered at a particular timing with ongoing tool
    calls, splitting them up incorrectly.

**Subagent tool calls not terminating when subthread tool calls were rejected**

: Due to a recent change in behavior, where we don't run inference
    automatically after tool call rejection, subagents could end up never
    signalling their termination, which led to the main thread never reaching an
    idle state.

    I confirmed this in both VSCode and the CLI: infinite spinners, indeed.

**Tool calls blocked on user not getting cancelled after user message**

: Due to how some tool calls require confirmation, like reading files outside
    the workspace or running some shell commands, in combination how we represent
    and track termination of tools, there's a possibility for such tools to be
    resumed and then, after an immediate user cancellation, not be properly
    cancelled. This leads to incorrect mutations of the thread data.

    I've not yet found the cause of this issue, but it's perfectly
    reproducible, so that's a start.
    

Furthermore, we were able to verify an older bug fix, where Anthropic's API
would send an invalid message with an empty tool use block array. That used to
get the agent into an infinite loop. With the fuzzer, we verified and improved
the old fix which had missed another case.

How about number of test runs and timeouts? Most of these bugs were found
almost immediately, i.e. within a second. The last one in the list above
takes longer, around a minute normally. We run a short version of each
fuzzer in every CI build, and longer runs on a nightly basis. This is
up for a lot of tuning and experimentation.

## Why the Entropy Buffer?

So why the entropy buffer instead of a seeded PRNG? The idea is to use that
buffer to mutate the test input, instead of just bombarding with random data
every time. If we can track which parts of the entropy was used where, we can
make those slices "smaller" or "bigger." We can use something like gradient
descent or simulated annealing to optimize inputs, maximizing some objective
function set by the fuzzer. Finally, we might be able to minimize inputs by
manipulating the entropy.

In case the JavaScript community gets some powerful fuzzing framework like
AFL+, that could also just be plugged in. Who knows, but I find this an
interesting approach that's worth exploring. I believe the entropy buffer
approach is also similar to how Hypothesis works under the hood. Someone please
correct me if that's not the case.

Anyhow, that's today's report from the generative testing mines. Cheers!
