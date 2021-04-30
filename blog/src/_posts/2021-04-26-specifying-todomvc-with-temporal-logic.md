---
layout: post
draft: true
title:  "Specifying State Machines with Temporal Logic"
author: Oskar Wickström
date: 2021-04-26
categories: programming
tags: []
excerpt: |
    ...
---

## Intro

Quickstrom uses linear temporal logic (LTL) for specifying web
applications. When explaining how it works, I've found that the basics
of LTL are quite intuitive to newcomers. On the other hand, it's not
so easy to see how to specify stateful systems using LTL. That's why
I'm sharing some of my learnings and ideas from the past year.

This post focuses on how to use LTL to specify state machines. It's a
brief overview that avoids going into too much detail. For more
information on how to test using such specifications, see the
Quickstrom documentation.

## An Extended LTL

We'll be using an LTL extended with three main components:

* an expression language for atomic propositions
* state selectors
* actions
 
The language is essentially a sketch of the future specification
language for Quickstrom. In this first post we'll focus only on the
LTL bits, and save the extensions for later posts.

A formula is a logical expression. We have the constants:

* `true`{.specstrom} (top)
* `false`{.specstrom} (bottom)

We combine formulae using the logical connectives, e.g:

* `&&`{.specstrom} (and)
* `||`{.specstrom} (or)
* `not`{.specstrom} (negation)
* `==>`{.specstrom} (implication)

## Temporal Operators

At the core of our language we have the notion of state. And being a
temporal logic, we talk about state over time. The formulae we've seen
so far do not deal with time. For that, we use temporal operators.

To show how the temporal operators work, I'll use diagrams to
visualize traces (sequences of states). A black circle denotes a state
in which the formula is true, and a white circle denotes a state where
the formula is false.

For example, let's say we have two formulae, `P`{.specstrom} and
`Q`{.specstrom}, and a five-state trace, where:

* `P`{.specstrom} is true in the first and second state
* `Q`{.specstrom} is true only in the second state

It would be visualized as follows:

```specstrom
P   ●───●───○───○───○
Q   ○───●───○───○───○
```

### Next

The `next`{.specstrom} operator takes a formula as an argument and evaluates it in
the next state.

```specstrom
P        ○───●───○───○───○
next P   ●───○───○───○───○
```

The `next`{.specstrom} operator is relative to the current state, not the first
state in the trace. This means that we can nest `next`{.specstrom}s to reach
further into the future.

```specstrom
P               ○───○───●───●───○
next P          ○───●───●───○───○
next (next P)   ●───●───○───○───○
```

### Next for State Transitions

All right, time for a more concrete example, something we'll evolve
throughout this blog post. Let's say we have a formula
`gdprConsentIsVisible`{.specstrom} which is true when the GDPR consent
screen is visible. We specify that the screen should be visible in the
current and next state like so:

```specstrom
gdprConsentIsVisible && next gdprConsentIsVisible
```

A pair of subsequent states is called a *step*. When specifying state
machines, we use the `next`{.specstrom} operator to describe state transitions. A
state transition formula is a logical predicate on a step.

In the GDPR example above, we said that the consent screen should stay
visible in both states of the step. If we want to describe a state
*change*, we can say:

```specstrom
gdprConsentIsVisible && next (not gdprConsentIsVisible)
```

The formula describes a state transition from an visible to a hidden
consent screen.

### Always

But interesting state machines usually have more than one possible
transition, and interesting behaviors likely contain multiple steps.

While we could nest formulae containing the `next`{.specstrom} operator, we'd be
stuck with specifications only describing a constant and finite number
of transitions.

Consider the following, where we like to state that the GDPR consent
screen should always be visible:

```specstrom
gdprConsentIsVisible && next (gdprConsentIsVisible && next ...)
```

This doesn't work for state machines with cycles, i.e. with possibly
infinite traces, because we can only nest a finite number of
`next`{.specstrom} operators. We want state machine specifications
that describe any number of transitions.

This is where we pick up the `always`{.specstrom} operator. It takes a formula as
an argument, and it's true if the given formula is true in the current
state and in all future states.

```specstrom
P          ●───●───●───●───●
always P   ●───●───●───●───●
Q          ●───○───●───●───●
always Q   ○───○───●───●───●
```

Note how `always Q`{.specstrom} is true in the third state and onwards, because
that's when `Q`{.specstrom} becomes true in the current and all future states.

Let's revisit the always-visible consent screen specification. Instead
of trying to nest an infinite amount of `next`{.specstrom} formulae, we instead
say:

```.specstrom
always gdprConsentIsVisible
```

Neat! This is called an *invariant property*. Invariants are
assertions on individual states, and an invariant property says that
it must hold for every state in the trace.

### Always for State Machines

Now, let's up our game. To model a state machine, we can combine
transitions with disjunction (`||`{.specstrom}) and the
`always`{.specstrom} operator. First, we define the individual
transition formulae `open`{.specstrom} and `close`{.specstrom}:

```specstrom
let open = 
  not gdprConsentIsVisible && next gdprConsentIsVisible;

let close = 
  gdprConsentIsVisible && next (not gdprConsentIsVisible);
```

Our state machine formula says that it always transitions as described
by `open`{.specstrom} or `close`:

```specstrom
always (open || close)
```

Cool, we have a state machine specification! Note that this
specification only allows for transitions where the visibility of the
consent screen changes back and forth.

So far we've only seen examples of *safety properties*. Those are
properties that specify that "nothing bad happens". But we also want
to specify that systems somehow make progress. The following two
temporal operators let us specify *liveness properties*, i.e. "good
things eventually happen".

### Eventually

We've used `next`{.specstrom} to specify transitions, and
`always`{.specstrom} to specify invariants and state machines. But we
might also want to use liveness properties in our specifications. In
this case, we are not talking about specific steps, but rather
*goals*.

The temporal operator `eventually`{.specstrom} takes a formula as an
argument, and it's true if the given formula is true in the current or
any future state.

```specstrom
P              ○───○───○───○───○
eventually P   ○───○───○───○───○
Q              ○───○───○───○───●
eventually Q   ●───●───●───●───●
```

For instance, we could say that the consent screen should initially be
visible and eventually be hidden:

```specstrom
gdprConsentIsVisible && eventually (not gdprConsentIsVisible)
```

This doesn't say that it *stays* hidden. It may become visible again,
and our specification would allow that. To specify that it should stay
hidden, we use a combination of `eventually`{.specstrom} and
`always`{.specstrom}:

```specstrom
gdprConsentIsVisible && eventually (always (not gdprConsentIsVisible))
```

Let's look at a diagram to understand this combination of temporal
operators better:

```specstrom
P                       ○───○───●───●───○
eventually (always P)   ○───○───○───○───○
Q                       ○───○───●───●───●
eventually (always Q)   ●───●───●───●───●
```

The formula `eventually (always P)`{.specstrom} is not true in any
state, because `P` never starts being true forever. The other formula,
`eventually (always Q)`{.specstrom}, is true in all states because `Q`
becomes true forever in the third state.

### Until

The last temporal operator I want to discuss is `until`{.specstrom}:

```specstrom
P           ●───●───○───○───○
Q           ○───○───●───●───●
P until Q   ●───●───●───●───●
```


It's more powerful than `always`{.specstrom} and
`eventually`{.specstrom}, and they can both be defined using
`until`{.specstrom}:

```specstrom
always P = P until false

eventually P = true until P
```

<!--
Future stuff:

* Formulae
  * Atomic propositions
  * Implicit lifting
* State-dependence
  * State selectors
  * Objects
* Actions
  * Preconditions
* Events 
  * They are actions
  * Postconditions
* What happened?
  * The `happened`{.specstrom} binding is a list of actions or events that happened between the last and the current state

-->
