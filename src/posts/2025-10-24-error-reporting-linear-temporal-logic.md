---
title: "Error Reporting in Linear Temporal Logic" 
date: "October 24, 2025"
author: "Oskar Wickström"
---

* Intro
    * Logic errors, unsat
    * Linear temporal logic, QuickLTL, and Quickstrom
    * Why error reporting matters
    * Error Reporting Logic reference and summary
* Picostrom and error reporting
    * QuickLTL recap
    * Introduce picostrom-rs
        * Learning Rust, be gentle
    * Motivating examples
* Plain text only?
    * Maybe show some huge error with lots of parts, hard to parse
    * Show the diagram example (bottom)
    * Other wild ideas
* Things left out
    * Implication; it'd be nice if you could trace _why_ some subformula is even relevant. A common pattern in state machine specs and other safety properties is:

        $$
        \text{always}_n(A \implies (B \land \text{next}_t(C)))
        $$

        If $B$ or $C$ are false, it'd be useful with an error also including the antecedent:

        > [...] because A, B in state 0 and C in state 1 [...]


    * `next (always X)` only shows inner error when failed
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


## Diagrams

Let's say we have a failing property like the following:

$$\text{next}_d(\text{next}_d(\text{always}_8(B < C)))$$

The textual error might be:

> **Definitely false:** as of state 2, it must always be the case that B is greater
> than C, but in state 6, B (13) is not greater than C (15)

But we could also draw a diagram, using information from the collected states:

<object data="/assets/ltl-error-reporting/always.svg" type="image/svg+xml" width="540px">
    <img src="/assets/ltl-error-reporting/always.svg" width="540px" />
</object>

Or for a liveness property like
$\text{next}_d(\text{next}_d(\text{eventually}_7(B = C)))$, where there is no
counterexample at a particular state, we could draw a diagram showing how we
give up after eight states:

<object data="/assets/ltl-error-reporting/eventually.svg" type="image/svg+xml" width="540px">
    <img src="/assets/ltl-error-reporting/eventually.svg" width="540px" />
</object>


