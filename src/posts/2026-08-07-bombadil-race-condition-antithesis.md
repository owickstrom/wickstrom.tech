---
title: "How Antithesis Found and Explained a Bombadil Race Condition"
date: "August 7, 2026"
author: "Oskar Wickström"
---


*Disclosure: I'm the original author and lead for the Bombadil project at Antithesis.*

[Carlos Catala](https://catala.dev/), interning at Antithesis and working on
testing our webapps, found a strange crash while I was out camping. The tests
he works on runs Antithesis' webapp *inside Antithesis* and explores it using
Bombadil. That surfaced a particular sequence of events leading up to the
following crash (logs are simplified for readability):

```
state machine error: unhandled transition: Running(10) + ActionAccepted(Click("Filters"))
```

What does this mean? To understand it, we need to back up a little.

The heart of the browser *driver* is a finite state machine, an abstract model
of a tab in Chrome or Chromium, and how we're interacting with it over CDP.
This is necessary due to how you can only invoke certain commands in certain
browser states, or it might hang or behave in other confounding ways. In our
state machine, some transitions are valid, and those are handled in a big
`match (state, event) { ... }` expression that returns a new state. Invalid
transitions are covered by the last branch that returns an `Err` with a message
like the above. 

There's another important component involved which is called the *runner*. It
decides the control flow of the test, and uses the driver to run the system
under test. The driver emits a state, and the runner decides what action to
take in that state. It hands back the action to the driver so that it can apply
it. The browser driver actually pauses the JS runtime when capturing a state,
and doesn't resume execution until the driver has given back the next action.

Hence, the `ActionAccepted` event (which is when the driver gets a new action
from the runner) is not valid in the `Running` state. It really is only valid
in the `Paused` state. How could this happen? The driver and runner are supposed
to operate in lock-step, where the driver is only in control while the JS runtime
is paused. This was the nut to crack.

When Carlos told me about this during my PTO, my camping brain was definitely
not helpful, so I let it rest until I was back at work. Staring at logs and
code also didn't help much, except for a poorly conceived theory about what
could be going on, mislead by the presence of [network
faults](https://antithesis.com/docs/product/fault_injection/fault_types/#network-faults)
in one test where we saw the bug. We tried but failed to reproduce this
locally, so I decided to find a simpler test subject and, crucially, try to
reproduce it in Antithesis.

## A Simple Reproduction with Causality Analysis

To reduce possible confusion I wanted to first see if I could trigger the same
bug in Bombadil with a simpler system under test. I found a Dockerized version
of [the-internet](https://the-internet.herokuapp.com/), an example application
by Sauce Labs made for automated browser testing. Based on what we had seen
before, I wrote a very small specification that only performs three actions:

```typescript
export * from "@antithesishq/bombadil/browser/defaults/properties";
import { weighted } from "@antithesishq/bombadil/browser";
import {
  back,
  forward,
  clicks,
} from "@antithesishq/bombadil/browser/defaults/actions";

export const navigation = weighted([
  [1, clicks],
  [1, back],
  [1, forward]
]);
```

In addition to finding various bugs in the Ruby code, it also immediately
reproduced the Bombadil bug! Time to dig in. I went through a few instances and
started to see a pattern. In addition, this gave me an excellent opportunity to
try out [Causality
Analysis](https://antithesis.com/docs/product/debugging/causality_analysis/), a
mind-bending feature for learning what behavior increases the likelihood of a
bug appearing. Here's the report I got:

![The causality analysis report plots the likelihood and helps you understand
what events caused a certain bug.](/assets/causality-analysis.webp)

As you may see in the chart, there are three points where the probability
increases markedly. Correlating those with the logs (which is easy, you see the
bumps highlighted in the log too), I could find the chain leading up to the bug:

1. Click a link that navigates to `/frames` (probability goes to 20%)
1. On that page, click a link that navigates to `/iframe` (probability goes to 47%)
1. Go back (I was a bit surprised that this didn't bump probability noticeably)
1. Go forward (probability goes to 99.98%)

After the last "Go forward" action, it's a done deal. Digging into the details
in between these points, I could then see how going back to a page which
immediately throws an exception, after restoring from
[bfcache](https://developer.mozilla.org/en-US/docs/Glossary/bfcache), takes a
different path than most page loads do.

## The Bug

When going back or forward in history, the state machine immediately
transitions to `Running`, waiting for confirmation by the browser to know what
actually happened. Some pages prevent bfcache through JavaScript or HTTP
headers, so a history navigation can also end up as a full page load with new
HTTP requests. When the page is confirmed to have been restored from bfcache,
the page and thus the state machine model skips the `Loading` state entirely,
and goes straight to `Running`.

In this case, that was followed by the exception being thrown immediately (you
can see it in the bottom of the screenshot above), which triggers another path:
exceptions thrown while in `Running` aren't collected and reported at a later
state capture, as is the case for the `Loading` state --- they immediately
pause the runtime and trigger a new state read. This in turn let the runner
pick a new action to perform. But the CDP event that confirms that the page was
restored from bfcache hadn't arrived yet.

This is a race between the asynchronous communication of runner and browser
state machine, selecting the next action, and the CDP event sent by Chromium
over a WebSocket confirming the bfcache restore. The unfortunate ordering that
triggers the bug is:

1. Back or forward, restoring from bfcache
2. Exception thrown on restored page, pausing the debugger and capturing a new
   state
3. Runner picking a new action and passing it back to the browser state machine
4. Confirmation of bfcache restore coming in, transition state machine to
   `Running`
5. State machine applying the `ActionAccepted` event, which, as we noted in the
   beginning of this excursion, is not valid when in `Running`

This sequence causes the crash. It only happens with bfcache-enabled history
navigation to a page with an exception being thrown before the confirmation
from Chromium comes back. Antithesis' deterministic fuzzing environment messes
with process scheduling and action selection and discovers this sequence
reliably.

## In Conclusion

I'm not going to describe the fix in too much detail, but it involves a more
robust use of the already existing `Generation` value in the browser state
machine. It's a monotonically increasing number that is used to detect stale
timeouts and actions, so that the state machine can discard them. The browser
state now carries the current generation, and that is threaded through the
runner's action selection process, so that a stale action (as in step 5 above)
can be safely ignored. Additionally, some transitions now increment the
generation in cases where all existing timeouts or actions are stale.

I've had bugs before of this kind, and understanding them from starting at
logs, let alone reproducing them, has been painful. This, in contrast, feels a
bit like cheating. Moreover, this extends to race conditions and complicated
bugs in the webapps themselves, which makes me very excited about continued
work on the Bombadil and Antithesis integration.
