---
layout: post
draft: true
title:  "Specifying State Machines with Temporal Logic"
author: Oskar Wickström
date: 2021-04-26
categories: general
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
language for Quickstrom.

A formula is a logical expression. We have the constants:

* `true` (top)
* `false` (bottom)

We combine formulae using the logical connectives, e.g:

* `&&` (and)
* `||` (or)
* `not` (negation)
* `==>` (implication)

## Temporal Operators

At the core of our language we have the notion of state. And being a
temporal logic, we talk about state over time. The formulae we've seen
so far do not deal with time. For that, we use temporal operators.

### Next

The `next` operator takes a formula as an argument and evaluates it
in the next state.

Let's say we have a formula `gdprConsentIsVisible` which is true when
the GDPR consent screen is visible. We specify that the screen should
be visible in the *current* and in the *next* state like so:

```
gdprConsentIsVisible && next gdprConsentIsVisible
```

A pair of subsequent states is called a *step*. When specifying state
machines, we use the `next` operator to describe state transitions. A
state transition formula is a logical predicate on a step.

In the GDPR example above, we said that the consent screen should stay
visible in both states of the step. If we want to describe a state
*change*, we can say:

```
gdprConsentIsVisible && next (not gdprConsentIsVisible)
```

The formula describes a state transition from an visible to a hidden
consent screen.

### Always

But interesting state machines usually have more than one possible
transition, and interesting behaviors likely contain multiple steps.

While we could nest expressions containing the `next` operator, we'd
be stuck with specifications only describing a constant and finite
number of transitions.

Consider the following, where we like to state that the GDPR consent
screen should always be visible:

```
gdprConsentIsVisible && next (gdprConsentIsVisible && next ...)
```

This doesn't work for state machines with cycles, because we can only
nest a finite number of `next` operators. We want state machine
specifications that describe any number of transitions.

This is where we pick up the `always` operator. It takes a formula as
an argument, and it's true if the given formula is true in the current
state and in all future states.

Let's revisit the always-visible consent screen specification. Now
that we know about the `always` operator, we instead say:

```
always gdprConsentIsVisible
```

Neat! (A website always showing a GDPR consent is perhaps not so neat,
though.)

Now, let's up our game. To model a state machine, we can combine
transitions with `||` and the `always` operator. First, we define the
individual transition formulae `open` and `close`:

```
let open = not gdprConsentIsVisible && next gdprConsentIsVisible;

let close = gdprConsentIsVisible && next (not gdprConsentIsVisible);
```

Our state machine formula says that it always transitions as described
by `open` or `close`:

```
always (open || close)
```

Note that this specification only allows for transitions where the
visibility of the consent screen changes.

So far we've only seen examples of *safety properties*. Those are
properties that specify that "nothing bad happens". When talking only
about a single state, they're often called *invariants*.

But we also want to specify that systems somehow make progress. The
following two temporal operators let us specify *liveness properties*,
i.e. "good things eventually happen".

### Eventually

We've used `next` to specify transitions, and `always` to specify
invariants and state machines. But we might also want to use liveness
properties in our specifications. In this case, we are not talking
about specific steps, but rather *goals*.

The temporal operator `eventually` takes a formula as an argument, and
it's true if the given formula is true in the current or any future
state.

For instance, we could say that the consent screen should initially be
visible and eventually be hidden:

```
gdprConsentIsVisible && next (not gdprConsentIsVisible)
```

### Until

...

## Formulae

  * Atomic propositions
  * Implicit lifting

## State-dependence

  * State selectors
  * Objects

## Actions

  * Preconditions

## Events 

  * They are actions
  * Postconditions

## What happened?

  * The `happened` binding is a list of actions or events that happened between the last and the current state

## Summary
  * ...

