---
title: "Finding Bugs in a Coding Agent with Lightweight DST" 
date: "August 28, 2025"
author: "Oskar Wickström"
---

[Amp](https://ampcode.com/) is a coding agent which I've been working on the
last six months at Sourcegraph. And in the last couple of weeks, I've been
building on a testing rig inspired by [Determinstic Simulation
Testing](https://github.com/ivanyu/awesome-deterministic-simulation-testing)
(DST) to test the most crucial parts of the system.

The goal is to get one of Amp's most central pieces, the _ThreadWorker_, under
heavy scrutiny. We've had a few perplexing bug reports, where users experienced
corrupted [threads](https://ampcode.com/manual#threads), LLM API errors from
invalid tool calls, and more vague issues like "it seems like it's spinning
forever." Reproducing such problems manually is usually somewhere between
impractical and impossible. I want to reproduce them deterministically, and in
a way were we can debug and fix them. And beyond the known ones, I'd like to find
the currently unknown ones before our users hit them.

Generative testing to the rescue!

## Approach: Lightweight DST in TypeScript

Amp is written in TypeScript, which is an ecosystem currently not drowning in
fuzzing tools. My starting point was using
[`jsfuzz`](https://www.npmjs.com/package/jsfuzz), which I hadn't used before
but it looked promising. However, I had a bunch of problems getting it to
run together with our Bun stack, so I decided to build something from scratch
for our purposes.

I borrowed an idea I got from [matklad](https://matklad.github.io/) last year:
instead of passing a seeded PRNG to generate test input, we generate an entropy
[`Buffer`](https://bun.com/docs/api/binary-data#buffer) with random contents,
and track our position in that array with a cursor. Drawing a random byte
"consumes" the byte at the current position and increments the cursor. We don't
know up-front how many bytes we need for a given fuzzer, so the entropy buffer
grows dynamically when needed. This, together with a bunch of methods for
drawing different types of values, is packaged up in an `Entropy` class:

```typescript
class Entropy {
    random(count): UInt8Array { ... }
    randomRange(minIncl: number, maxExcl: number): number { ... }
    // ... lots of other stuff
}
```

A fuzzer is an ES module written in TypeScript, exporting a single function:

```typescript
export async function fuzz(logger: Logger, entropy: Entropy) {

}
```

The `Logger` isn't important; it's an instance of the centralized logging
system we use, and for the fuzzing framework to control log levels it has to be
passed in. The `Entropy` is the class we just covered. Any exception thrown by
`fuzz` is considered a test failure. We use the `node:assert` module for our
test assertions.

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
nondeterminstic components, and we install [fake
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
* all messages pairs involving tools calls are valid according to Anthropic's API specification
* all tool calls have settled in expected terminal states

Some of these are targeted at specific known bugs, while some are more general
but have found bugs we did not expect. Let's dig into the findings!

## Results

Given I've been working on this for about a week in total, I'm surprised (as
always with generative testing) by the outcome. Here are some issues the fuzzer
found:

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
    

Finally, we were able to verify an older bug fix, where Anthropic's API would
send an invalid message with an empty tool use block array. That used to get
the agent into an infinite loop. With the fuzzer, we verified and improved the
old fix.

## Future Work

I've left you waiting for too long: why this entropy buffer instead of just a
seed? Right, so the idea is to use that string to mutate the inputs, instead
of just bombarding with new random data every time. If we can track which parts
of the entropy was used where, we can make those slices "smaller" or "bigger".
We can use something like gradient descent or simulated annealing to optimize
inputs, maximizing some objective function set by the fuzzer. Finally, we might
be able to minimize inputs by manipulating the entropy.

In case the Javascript community gets some powerful fuzzing framework like
AFL+, that could also just be plugged in. Who knows, but I find this an
interesting approach that's worth exploring. I believe this is also similar to
how Hypothesis works? Someone please correct me if that's wrong.

Anyhow, that's today's report from the generative testing mines. See ya!
