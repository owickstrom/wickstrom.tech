---
title: "Computer Says No: Error Reporting for Linear Temporal Logic" 
date: "October 24, 2025"
author: "Oskar Wickström"
---

[Quickstrom](https://quickstrom.io/) uses
[QuickLTL](https://arxiv.org/abs/2203.11532), a linear temporal logic with
finite traces, to specify and test web applications. As with many other logic
systems, when a formula evaluates to false --- like when a counterexample to a
safety property is found or a liveness property cannot be shown to hold ---
_the computer says no_. Hunting down complex bugs in stateful web applications
then comes down to staring at the specification alongside a trace of states and
screenshots, hoping that you somehow can pin down what went wrong. It's not
great.

Instead, we should have helpful error messages explaining _why_ a property does
not hold; which parts of the specification failed and which concrete values
from the trace were involved. I started exploring this space a few years ago
when I worked actively on Quickstrom, but for some reason it went on the shelf
half-finished. Time to tie up the loose ends!

The starting point was _Picostrom_, a minimal Haskell version of the checker in
Quickstrom, and [Error Reporting
Logic](https://www.cs.cmu.edu/~cchristo/docs/jaspan-ASE08.pdf), a paper
introducing a way of rendering natural-language messages to explain
propositional logic counterexamples.

I've now ported it over to Rust, for _reasons_. Mostly because I wanted to see
what it turned into. I'm still on the rookie side of the Rust scale, so be
gentle. I've also extended it to handle nested temporal operators better, and
added support for implication. The code is available [on
Codeberg](https://codeberg.org/owi/picostrom-rs) under the MIT license.

Between the start of my work and picking it back up now, [A Language for
Explaining Counterexamples](https://doi.org/10.4230/OASIcs.SLATE.2024.11) was
published, which looks closely related, but focused on model checking with LTL
and CTL. If you're interested in other related work, check out [A Systematic
Literature Review on Counterexample Explanation in Model
Checking](https://arxiv.org/abs/2201.03061).

All right, let's dive in!

## QuickLTL

A quick recap on QuickLTL is in order. It's a four-valued logic, meaning that a
formula evaluates to one of these values:

* $\text{definitely true}$
* $\text{definitely false}$
* $\text{probably true}$
* $\text{probably false}$

It extends propositional logic with temporal operators, much like LTL:

$\text{next}_d(P)$

: $P$ must hold in the next state, demanding a next state is available.

$\text{next}_f(P)$

: $P$ must hold in the next state, defaulting to $\text{definitely false}$ if no next
state is available.

$\text{next}_t(P)$

: $P$ must hold in the next state, defaulting to $\text{probably true}$ if no next state
is available.

$\text{eventually}_N(P)$

: $P$ must hold in the current or a future state. It demands at least $N$
states, evaluating on all available states, and finally defaulting to
$\text{probably false}$.

$\text{always}_N(P)$

: $P$ must hold in the current and all future states. It demands at least $N$
states, evaluating on all available states, and finally defaulting to
$\text{probably true}$.

You can think of $\text{eventually}_N(P)$ as unfolding into a sequence of $N$
nested $\text{next}_D$ wrapping an infinite sequence of $\text{next}_F$,
connected by $\lor$:

$$
P \lor \text{next}_D (P \lor \text{next}_D (\ \ \ldots\ \ P \lor \text{next}_F (\lor \text{next}_F (\ \ \ldots\ \ ))\ \ \ldots\ \ ))
$$

And similarly, $\text{always}_N(P)$ can be thought of as $N$ nested
$\text{next}_D$ wrapping an infinite sequence of $\text{next}_T$, all connected
by $\land$.

* Picostrom and error reporting
    * QuickLTL recap
    * Introduce picostrom-rs
        * Learning Rust, be gentle
    * Motivating examples
* Plain text only?
    * Maybe show some huge error with lots of parts, hard to parse
    * Show the diagram example (bottom)
    * Other wild ideas
* Short-circuiting behavior
* Differences from ERL:
    - no splitting/joining errors, instead a `Problem` AST
    - no responsible objects, free variables
* Things left out
    * Not exactly left out, but QuickLTL suffers from and infinite loop issue, where a formula like the following causes the evaluation loop to never terminate:

        $$\text{always}_10(\text{eventually}_5(X))$$

      This is because the outer _always_ consumes the extra states demanded by
      the inner _eventually_, causing new _eventually_ chains to be spun off,
      demanding yet more states, and so on. This is tricky to solve in the
      logic itself, but could be possibly dealt with using a global limit on
      the number of states.
    * I've focused on QuickLTL, which deals with finite traces. What about
      infinite temporal logics? I'm guessing this could be adapted but I
      haven't tried.

## Small Errors, Short Tests

Let's consider a conjunction of two invariants. We could of course combine the
two atomic propositions with conjunction inside a single $\text{always}(...)$, but
in this case we have the formula:

$$\text{always}(A < 3) \land \text{always}(B < C)$$

An error message, where both invariants fail, might look the following:

> **Definitely false:**
> it must always be the case that A is less than 3 and it must always be the case that B is greater than C, but
> A=3 in state 3 and B=0 in state 3

If only the second invariant ($B < C$) fails, we get a smaller error:

> **Definitely false:** it must always be the case that B is greater than C, but B=0 and C=0 in state 0

And, crucially, if one of the invariants fail before the other, we get a
smaller error, ignoring the other invariant. While single-state conjuctions
evaluate both sides, possibly creating composite errors, conjunctions over time
short-circuit to reduce testing time.

## Implication

You can trace _why_ some subformula is relevant when using implication. A
common pattern in state machine specs and other safety properties is:

$$
\text{always}_N(A > 0 \implies (B > 5 \land \text{next}_t(C < 10)))
$$

If $B$ or $C$ are false, the error includes the antecedent:

> **Definitely false:** B must be greater than 5 and in the next state, C must be less than 10 since A is greater than 0, [...]

## Diagrams

Let's say we have a failing property like the following:

$$\text{next}_d(\text{always}_8(B < C))$$

The textual error might be:

> **Definitely false:** in the next state, it must always be the case that B is
> greater than C, but B=13 and C=15 in state 6

But we could also draw a diagram, using information from the collected states:

<object data="/assets/ltl-error-reporting/always.svg" type="image/svg+xml" width="540px">
    <img src="/assets/ltl-error-reporting/always.svg" width="540px" />
</object>

Or for a liveness property like
$\text{next}_d(\text{eventually}_8(B = C))$, where there is no
counterexample at a particular state, we could draw a diagram showing how we
give up after some time:

<object data="/assets/ltl-error-reporting/eventually.svg" type="image/svg+xml" width="540px">
    <img src="/assets/ltl-error-reporting/eventually.svg" width="540px" />
</object>
