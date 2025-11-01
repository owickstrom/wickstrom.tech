---
title: "Computer Says No: Error Reporting for LTL" 
date: "October 24, 2025"
author: "Oskar Wickström"
---


[Quickstrom](https://quickstrom.io/) is a property-based testing tool for web
applications, using [QuickLTL](https://arxiv.org/abs/2203.11532) for specifying
the intended behavior. QuickLTL is a linear temporal logic (LTL) over finite
traces, especially suited for testing. As with many other logic systems, when a
formula evaluates to false --- like when a counterexample to a safety property
is found or a liveness property cannot be shown to hold --- the computer says
no. That is, you get "false" or "test failed", perhaps along with a trace.
Understanding complex bugs in stateful systems then comes down to staring at
the specification alongside the trace, hoping you can somehow pin down what
went wrong. It's not great.

Instead, we should have helpful error messages explaining _why_ a property does
not hold; which parts of the specification failed and which concrete values
from the trace were involved. Not `false`, `unsat`, or even `assertion error: x
!= y`. We should get the full story. I started exploring this space a few years
ago when I worked actively on Quickstrom, but for some reason it went on the
shelf half-finished. Time to tie up the loose ends!

The starting point was _Picostrom_, a minimal Haskell version of the checker in
Quickstrom, and [Error Reporting
Logic](https://www.cs.cmu.edu/~cchristo/docs/jaspan-ASE08.pdf) (ERL), a paper
introducing a way of rendering natural-language messages to explain
propositional logic counterexamples. I ported it to Rust mostly to see what it
turned into, and extended it with error reporting supporting temporal
operators. I'm still on the rookie side of the Rust scale, so be gentle! The
code is available at
[codeberg.org/owi/picostrom-rs](https://codeberg.org/owi/picostrom-rs) under
the MIT license.

Between the start of my work and picking it back up now, [A Language for
Explaining Counterexamples](https://doi.org/10.4230/OASIcs.SLATE.2024.11) was
published, which looks closely related, although it's focused on model checking
with CTL. If you're interested in other related work, check out [A Systematic
Literature Review on Counterexample Explanation in Model
Checking](https://arxiv.org/abs/2201.03061).

All right, let's dive in!

## QuickLTL and Picostrom

A quick recap on QuickLTL is in order before we go into the Picostrom code.
QuickLTL operates on _finite_ traces, making it suitable for testing. It's a
four-valued logic, meaning that a formula evaluates to one of these values:

* $\text{definitely true}$
* $\text{definitely false}$
* $\text{probably true}$
* $\text{probably false}$

It extends propositional logic with temporal operators, much like LTL:

$\text{next}_d(P)$

: $P$ must hold in the next state, demanding a next state is available. This
_forces_ the evaluator to draw a next state.

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
nested $\text{next}_d$, wrapping an infinite sequence of $\text{next}_f$,
connected by $\lor$:

$$
P \lor \text{next}_D (P \lor \text{next}_D (\ \ \ldots\ \ P \lor \text{next}_F (P \lor \text{next}_F (\ \ \ldots\ \ ))\ \ \ldots\ \ ))
$$

Or even better, let's define it inductively with a coinductive base case:

$$
\begin{align}
\text{eventually}_0(P) & = P \lor \text{next}_F(\text{eventually}_0(P)) \\
\text{eventually}_(N + 1)(P) & = P \lor \text{next}_D(\text{eventually}_N(P)) \\
\end{align}
$$

And similarly, $\text{always}_N(P)$ can be defined as:

$$
\begin{align}
\text{always}_0(P) & = P \land \text{next}_T(\text{always}_0(P)) \\
\text{always}_(N + 1)(P) & = P \land \text{next}_D(\text{always}_N(P)) \\
\end{align}
$$

This is essentially how the evaluator expands these temporal operators, but for
error reporting reasons, not exactly.

Finally, there are _atoms_, which are domain-specific expressions embedded in
the AST, evaluating to $\top$ or $\bot$. The AST is parameterized on the atom
type, so you can plug in an atom language of choice. An atom type must
implement the `Atom` trait, which in simplified form looks like this:

```rust
trait Atom {
    type State;
    fn eval(&self, state: &Self::State) -> bool;
    fn render(
        &self, 
        mode: TextMode, 
        negated: bool,
    ) -> String;
    fn render_actual(
        &self, 
        negated: bool, 
        state: &Self::State,
    ) -> String;
}
```

For testing the
checker, and for this blog post, I'm using the following atom type:

```rust
enum TestAtom {
    Literal(u64),
    Select(Identifier),
    Equals(Box<TestAtom>, Box<TestAtom>),
    LessThan(Box<TestAtom>, Box<TestAtom>),
    GreaterThan(Box<TestAtom>, Box<TestAtom>),
}

enum Identifier {
    A,
    B,
    C,
}
```


## Evaluation

The first step, like in ERL, is transforming the formula into [negation normal
form](https://en.wikipedia.org/wiki/Negation_normal_form) (NNF), which means
pushing down all negations into the _atoms_:

```rust
enum Formula<Atom> {
    Atomic {
        negated: bool,
        atom: Atom,
    },
    // There's no `Not` variant here!
    ...
}
```

This makes it much easier to construct readable sentences, in addition to
another important upside. The NNF representation is the one used by the
evaluator internally. 

Next, the `eval` function takes an `Atom::State` and a `Formula`, and produces a
`Value`:

```rust
enum Value<'a, A: Atom> {
    True,
    False { problem: Problem<'a, A> },
    Residual(Residual<'a, A>),
}
```

A value is either an immediate $\top$ or $\bot$, meaning that we don't need to
evaluate on additional states, or a _residual_, which is like a description of
how to continue evaluating a formula when given a next state. Also note how the
`False` variant holds a `Problem`, which is what we'd report as
$\text{definitely false}$. The `True` variant doesn't need to hold any such
information, because due to NNF, it can't be negated and "turned into a
problem."

I won't cover every variants of the `Residual` type, but let's take one example:

```rust

pub enum Residual<'a, A: Atom> {
    // ...
    AndAlways {
        start: Numbered<&'a A::State>,
        left: Box<Residual<'a, A>>,
        right: Box<Residual<'a, A>>,
    },
    // ...
}
```

When such a value is returned, the evaluator checks if it's possible to stop at
this point, i.e. if there are no _demanding_ operators in the residual. If not
possible, it draws a new state and calls `step` on the residual. The `step`
function is analogous to `eval`, also returning a `Value`, but it operates on a
`Residual` rather than a `Formula`.

The `AndAlways` variant describes an ongoing evaluation of the $\text{always}$
operator, where the `left` and `right` residuals are the operands of $\land$ in
the inductive definition I described earlier. Similarly, it has variants for 
$\lor$, $\land$, $\implies$, $\text{next}$, $\text{eventually}$,
and a few others.

When the `stop` function deems it possible to stop evaluating, we get back a
value of this type:

```rust
enum Stop<'a, A: Atom> {
    True,
    False(Problem<'a, A>),
}
```

Those variants correspond to $\text{probably true}$ and  $\text{probably
false}$. In the false case, we get a `Problem` which we can render. Recall how
the `Value` type returned by `eval` and `step` also had `True` and `False`
variants? Those are the definite cases.

## Rendering Problems

The `Problem` type is a tree structure, mirroring the structure of the
evaluated formula, but only containing the parts of it that contributed to its
falsity.

```rust
enum Problem<'a, A: Atom> {
    And {
        left: Box<Problem<'a, A>>,
        right: Box<Problem<'a, A>>,
    },
    Or {
        left: Box<Problem<'a, A>>,
        right: Box<Problem<'a, A>>,
    },
    Always {
        state: Numbered<&'a A::State>,
        problem: Box<Problem<'a, A>>,
    },
    Eventually {
        state: Numbered<&'a A::State>,
        formula: Box<Formula<A>>,
    },
    // A bunch of others...
}
```

I've written a simple renderer that walks the `Problem` tree, constructing
English error messages. When hitting the atoms, it uses the `render` and `render_actual` methods from the `Atom` trait
 I showed you before.

The `mode` is very much like in the ERL paper, i.e. whether it should be
rendered in deontic (e.g. "x should equal 4") or indicative (e.g. "x equals
4") form:

```rust
enum TextMode {
    Deontic,
    Indicative,
}
```

The `render` method should render the atom according to the mode, and
`render_actual` should render relevant parts of the atom in a given state, like
its variable assignments.

With all these pieces in place, we can finally render some error messages! Let's
say we have this formula:

$$
\text{eventually}_10( B = 3 \land C = 4)
$$

If we run a test and never see such a state, the rendered error would be:

> **Probably false:** eventually B must equal 3 and C must equal 4, but it was not observed starting at state 0

Neat! This is the kind of error reporting I want for my stateful tests.

## Implication

You can trace _why_ some subformula is relevant by using implication. A
common pattern in state machine specs and other safety properties is:

$$
\text{precondition} \implies \text{before} \land \text{next}_t(\text{after})
$$

So, let's say we have this formula:

$$
\text{always}_N((A > 0) \implies (B > 5 \land \text{next}_t(C < 10)))
$$

If $B$ or $C$ are false, the error includes the antecedent:

> **Definitely false:** B must be greater than 5 and in the next state, C must be less than 10 since A is greater than 0, [...]

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

And, crucially, if one of the invariants fail before the other we also get a
smaller error, ignoring the other invariant. While single-state conjunctions
evaluate both sides, possibly creating composite errors, conjunctions over time
short-circuit to reduce testing time.

## Diagrams

Let's say we have a failing safety property like the following:

$$\text{next}_d(\text{always}_8(B < C))$$

The textual error might be:

> **Definitely false:** in the next state, it must always be the case that B is
> greater than C, but B=13 and C=15 in state 6

But with some tweaks we could also draw a diagram, using the `Problem` tree and
the collected states:

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

These are only sketches, but I think they show how the `Problem` data structure
can be used in many interesting ways. What other visualizations would be
possible? An interactive state space explorer could show how problems evolve as
you navigate across time. You could generate spreadsheets or HTML documents, or
maybe even annotate the relevant source code of some system-under-test? I think
it depends a lot on the domain this is applied to.

## No Loose Ends

It's been great to finally finish this work! I've had a lot of fun working
through the various head-scratchers in the evaluator, getting strange
combinations of temporal operators to render readable error messages. I also
enjoyed drawing the diagrams, and _almost_ nerd-sniped myself into automating
that. Maybe another day. I hope this is interesting or even useful to someone
out there. LTL is really cool and should be used more!

The code, including many rendering tests cases, is available at
[codeberg.org/owi/picostrom-rs](https://codeberg.org/owi/picostrom-rs).

_A special thanks goes to [Divyanshu Ranjan](https://rdivyanshu.github.io/) for reviewing
a draft of this post._
