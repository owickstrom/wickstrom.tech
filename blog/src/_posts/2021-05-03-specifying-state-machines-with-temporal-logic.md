---
layout: post
title:  "Specifying State Machines with Temporal Logic"
author: Oskar Wickström
date: 2021-05-03
categories: programming
tags: []
excerpt: |
    When explaining how Quickstrom works, I've found that it's not 
    obvious how to specify real-world systems using LTL. In this
    post I'm sharing some of my learnings and ideas.
documentclass: scrartcl
papersize: a5
header-includes:
 - \usepackage{csquotes}
 - \usepackage{ebgaramond-maths}
 - \usepackage[scale=0.8]{FiraMono}
---

[Quickstrom](https://quickstrom.io) uses linear temporal logic (LTL)
for specifying web applications. When explaining how it works, I've
found that the basics of LTL are intuitive to newcomers. On the other
hand, it's not obvious how to specify real-world systems using
LTL. That's why I'm sharing some of my learnings and ideas from the
past year in the form of blog posts.

This post focuses on how to use LTL to specify systems in terms of
state machines. It's a brief overview that avoids going into too much
detail. For more information on how to test web applications using
such specifications, see [the Quickstrom
documentation](https://docs.quickstrom.io).

To avoid possible confusion, I want to start by pointing out that a
state machine specification in this context is not the same as a model
in TLA+ (or similar modeling languages.) We're not building a model to
prove or check properties against.  Rather, we're defining properties
in terms of state machine transitions, and the end goal is to test
actual system behavior (e.g. web applications, desktop applications,
APIs) by checking that recorded traces match our specifications.

## Linear Temporal Logic

In this post, we'll be using an LTL language. It's a sketch of a
future specification language for Quickstrom.

A *formula* (plural *formulae*) is a logical expression that evaluates
to true or false. We have the constants:

* `true`{.specstrom}
* `false`{.specstrom}

We combine formulae using the logical connectives, e.g:

* `&&`{.specstrom}
* `||`{.specstrom}
* `not`{.specstrom}
* `==>`{.specstrom}

The `==>`{.specstrom} operator is *implication*. So far we have
propositional logic, but we need a few more things.

## Temporal Operators

At the core of our language we have the notion of state. Systems
change state over time, and we'd like to express that in our
specifications. But the formulae we've seen so far do not deal with
time. For that, we use temporal operators.

To illustrate how the temporal operators work, I'll use diagrams to
visualize *traces* (sequences of states). A black circle denotes a
state in which the formula is true, and a white circle denotes a state
where the formula is false.

For example, let's say we have two formulae, `P`{.specstrom} and
`Q`{.specstrom}, where:

* `P`{.specstrom} is true in the first and second state
* `Q`{.specstrom} is true in the second state

Both formulae are false in all other states. The formulae and trace
would be visualized as follows:

```specstrom
P   ●───●───○
Q   ○───●───○
```

Note that in these diagrams, we assume that the last state repeats
forever. This might seem a bit weird, but drawing an infinite number
of states is problematic.

### Next

The `next`{.specstrom} operator takes a formula as an argument and evaluates it in
the next state.

```specstrom
next P   ●───○───○
P        ○───●───○
```

The `next`{.specstrom} operator is relative to the current state, not the first
state in the trace. This means that we can nest `next`{.specstrom}s to reach
further into the future.

```specstrom
next (next P)   ●───●───○───○───○
next P          ○───●───●───○───○
P               ○───○───●───●───○
```

### Next for State Transitions

All right, time for a more concrete example, something we'll evolve
throughout this post. Let's say we have a formula
`gdprConsentIsVisible`{.specstrom} which is true when the GDPR consent
screen is visible. We specify that the screen should be visible in the
current and next state like so:

```specstrom
gdprConsentIsVisible && next gdprConsentIsVisible
```

A pair of consecutive states is called a *step*. When specifying state
machines, we use the `next`{.specstrom} operator to describe state transitions. A
state transition formula is a logical predicate on a step.

In the GDPR example above, we said that the consent screen should stay
visible in both states of the step. If we want to describe a state
change in the consent screen's visibility, we can say:

```specstrom
gdprConsentIsVisible && next (not gdprConsentIsVisible)
```

The formula describes a state transition from a visible to a hidden
consent screen.

### Always

But interesting state machines usually have more than one possible
transition, and interesting behaviors likely contain multiple steps.

While we could nest formulae containing the `next`{.specstrom}
operator, we'd be stuck with specifications only describing a finite
number of transitions.

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
always P   ●───●───●───●───●
P          ●───●───●───●───●
always Q   ○───○───●───●───●
Q          ●───○───●───●───●
```

Note how `always Q`{.specstrom} is true in the third state and onwards, because
that's when `Q`{.specstrom} becomes true in the current and all future states.

Let's revisit the always-visible consent screen specification. Instead
of trying to nest an infinite amount of `next`{.specstrom} formulae, we instead
say:

```specstrom
always gdprConsentIsVisible
```

Neat! This is called an *invariant property*. Invariants are
assertions on individual states, and an invariant property says that
it must hold for every state in the trace.

### Always for State Machines

Now, let's up our game. To specify the system as a state machine, we
can combine transitions with disjunction (`||`{.specstrom}) and the
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

We have a state machine specification! Note that this specification
only allows for transitions where the visibility of the consent screen
changes back and forth.

So far we've only seen examples of *safety properties*. Those are
properties that specify that "nothing bad happens." But we also want
to specify that systems somehow make progress. The following two
temporal operators let us specify *liveness properties*, i.e. "good
things eventually happen."

Quickstrom does not support liveness properties yet.[^1]

### Eventually

We've used `next`{.specstrom} to specify transitions, and
`always`{.specstrom} to specify invariants and state machines. But we
might also want to use liveness properties in our specifications. In
this case, we are not talking about specific steps, but rather
goals.

The temporal operator `eventually`{.specstrom} takes a formula as an
argument, and it's true if the given formula is true in the current or
any future state.

```specstrom
eventually P   ○───○───○───○───○
P              ○───○───○───○───○
eventually Q   ●───●───●───●───○
Q              ○───○───○───●───○
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
eventually (always P)   ○───○───○───○───○
P                       ○───○───●───●───○
eventually (always Q)   ●───●───●───●───●
Q                       ○───○───●───●───●
```

The formula `eventually (always P)`{.specstrom} is not true in any
state, because `P` never starts being true forever. The other formula,
`eventually (always Q)`{.specstrom}, is true in all states because `Q`
becomes true forever in the third state.

### Until

The last temporal operator I want to discuss is `until`{.specstrom}.
For `P until Q`{.specstrom} to be true, `P` must be true until `Q`
becomes true.

```specstrom
P until Q   ●───●───●───●───○
P           ●───●───○───○───○
Q           ○───○───●───●───○
```

Just as with the `eventually`{.specstrom} operator, the stop condition
(`Q`) doesn't have to stay true forever, but it has to be true at
least once.

The `until`{.specstrom} operator is more expressive than
`always`{.specstrom} and `eventually`{.specstrom}, and they can both
be defined using `until`{.specstrom}.[^2]

Anyway, let's get back to our running example. Suppose we have another
formula `supportChatVisible` that is true when the support chat button
is shown. We want to make sure it doesn't show up until after the GDPR
consent screen is closed:

```specstrom
not supportChatVisible until not gdprConsentIsVisible
```

The negations make it a bit harder to read, but it's equivalent to the
informal statement: "the support chat button is hidden at least until
the GDPR consent screen is hidden." It doesn't demand that the support
chat button is ever visible, though. For that, we instead say:

```specstrom
gdprConsentIsVisible 
  until (supportChatVisible && not gdprConsentIsVisible)
```

In this formula, `supportChatVisible` has to become true eventually,
and at that point the consent screen must be hidden.

### Until for State Machines

We can use the `until`{.specstrom} operator to define a state machine
formula where the final transition is more explicit.

Let's say we want to specify the GDPR consent screen more
rigorously. Suppose we already have the possible state transition
formulae defined:

* `allowCollectedData`
* `disallowCollectedData`
* `submit`

We can then put together the state machine formula:

```specstrom
let gdprConsentStateMachine = 
  gdprConsentIsVisible 
    && (allowCollectedData || disallowCollectedData) 
         until (submit && next (not gdprConsentIsVisible));
``` 

In this formula we allow any number of `allowCollectedData` or
`disallowCollectedData` transitions, until the final `submit`
resulting in a closed consent screen.

## What's next?

We've looked at some temporal operators in LTL, and how to use them to
specify state machines. I'm hoping this post has given you some ideas
and inspiration!

Another blog post worth checking out is [TLA+ Action
Properties](https://hillelwayne.com/post/action-properties/) by Hillel
Wayne. It's written specifically for TLA+, but most of the concepts
are applicable to LTL and Quickstrom-style specifications.

I intend to write follow-ups, covering atomic propositions, queries,
actions, and events. If you want to comment, there are threads on
[GitHub](https://github.com/quickstrom/quickstrom/discussions/101),
[Twitter](https://twitter.com/owickstrom/status/13891582598152355840),
and on
[Lobsters](https://lobste.rs/s/uifucu/specifying_state_machines_with_temporal). You
may also want to [sponsor my
work](https://github.com/sponsors/owickstrom).

*Thank you [Vitor Enes](https://twitter.com/vitorenesduarte), [Andrey
Mokhov](https://twitter.com/andreymokhov), [Pascal Poizat
](https://twitter.com/pascalpoizat), and [Liam
O'Connor](https://twitter.com/kamatsu8) for reviewing drafts of this
post.*

## Footnotes

[^1]: A future version of Quickstrom will use a different flavor of
    LTL tailored for testing, and that way support liveness
    properties.
[^2]: We can define `eventually P = true until P`{.specstrom}, and
    perhaps a bit harder to grasp, `always P = not (true until not
    P)`{.specstrom}. Or we could say `always P = not (eventually not
    P)`{.specstrom}.

<!-- Future stuff:

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


