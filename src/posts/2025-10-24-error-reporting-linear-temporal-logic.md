---
title: "Error Reporting in Linear Temporal Logic" 
date: "October 24, 2025"
author: "Oskar Wickström"
---

* Intro
    * Logic errors, unsat
    * Linear temporal logic, QuickLTL, and Quickstrom
    * Why error reporting matters
    * [Error Reporting Logic](https://www.cs.cmu.edu/~cchristo/docs/jaspan-ASE08.pdf) reference and summary
    * [A Systematic Literature Review on Counterexample Explanation in Model Checking](https://arxiv.org/abs/2201.03061)
    * [A Language for Explaining Counterexamples](https://drops.dagstuhl.de/entities/document/10.4230/OASIcs.SLATE.2024.11)
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
\text{always}_n(A > 0 \implies (B > 5 \land \text{next}_t(C < 10)))
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


