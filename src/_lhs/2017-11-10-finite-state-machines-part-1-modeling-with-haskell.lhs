Stateful programs often become complex beasts as they grow. Program
state incohesively spread across a bunch of variables, spuriously
guarded by even more variables, is what I refer to as *implicit
state*. When working with such code, we have to reconstruct a model
mentally, identifying possible states and transitions between them, to
modify the program with confidence. Even if a test suite can help, the
process is tedious and error-prone, and I insist we should have our
tools do the heavy lifting instead.

By teaching the type system about possible states and state
transitions in our program, it can verify that we follow our own
business rules, both when we write new code, and when we modify
existing code. It is not merely a process of asking the compiler "did
I do okay?" Our workflow can be a conversation with the compiler, a
process known as *type-driven development.* Moreover, the program
*encodes* the state machine as living machine-verified documentation.

After having given my talk at Code Mesh on this topic, and having
spent a lot of time researching and preparing examples, I want to
share the material in the form of a blog post series. Each post will
cover increasingly advanced techniques that give greater static
safety guarantees. That is not to say that the latter techniques are
inherently better, nor that they are the ones that you should
use. This series is meant as a small à la carte of event-driven state
machine encodings and type safety, where you can choose from the menu
based on your taste and budget. I will, however, present the
techniques in a linear form. Also, note that these posts do not claim
to exhaust all options for state machine encodings.

There are many trade-offs, including type safety and strictness,
implementation complexity, and how language, technique, and library
choices affect your team. Taking one step towards *explicit state*, in
an area where it leverages your project in doing so, can be the best
choice. You don't have to go nuts with type systems to use explicit
states in your program! Furthermore, most mainstream languages today
let you encode states as data types in some way.

![Our journey begins in the Valley of Programmer Death, deep in the
 lands of Implicit State. Follow along for as long as you like, and
 settle down in a place of your choice.](/assets/fsm-map.png)

This is the introductory post, in which I'll show the first step on
our way from implicit state and despair to writing stateful and
effectful programs with great confidence. We will use Haskell and
*algebraic data types* (ADTs) to encode possible states as data types.
You should be able to read and understand this post without knowing
much Haskell. If not, tell me, and I will try to explain better.

Finite-State Machines
=====================

First, we should have a clear understanding of what a finite-state
machine is. There are many variations and definitions, and I'm sure
you, especially if coming from an engineering background, have some
relation to state machines.

In general, a finite-state machine can be described as an abstract
machine with a finite set of states, being in one state at a time.
*Events* trigger state transitions; that is, the machine changes from
being in one state to being in another state. The machine defines a
set of legal transitions, often expressed as associations from a state
and event pair to a state.

For the domains we will be exploring, the [Erlang documentation's
definition](http://erlang.org/documentation/doc-4.8.2/doc/design_principles/fsm.html)
of a finite-state machine is simple and useful:

<blockquote>
<p>*State(S) &times; Event(E) &rarr; Actions (A), State(S')*</p>
<p>If we are in state S and the event E occurs, we should perform the
actions A and make a transition to the state S'.</p>
</blockquote>

That is the basis for the coming posts. I will not categorize as Mealy
or Moore machines, or use UML state charts, at least not to any
greater extent. Some diagrams will use the notation for hierarchical
state machines for convenience.

Example: Shopping Cart Checkout
===============================

The running example we will use in these posts is a *shopping cart
checkout*, modeled as an event-driven finite-state machine. This
stems from a real-world project I worked on, where the lack of
explicit states in code became a real problem as requirements
evolved. It's the use-case that inspired me to look for more robust
methods.

![The running example, a shopping cart checkout.](/generated/uml/checkout.svg)

As shown graphically in the state diagram above, we start in
"NoItems", selecting one or more items, staying in "HasItems", until
we begin the checkout. We enter the nested "Checkout" machine on the
"checkout" event. Modeling it as a hierarchically nested machine we
can have all its states accept the "cancel" event. We select and
confirm a card, and eventually place an order, if not canceled.

States and Events as Data Types
===============================

As this blog post is written as [a Literate Haskell
program](https://github.com/owickstrom/func-da-world/blob/master/src/_lhs/2017-11-10-finite-state-machines-part-1-modeling-with-haskell.lhs),
we begin with the module declaration and imports. We will use `Text`
instead of `String`, and `NonEmpty` lists. The two modules
`PaymentProvider` and `Checkout` hide some implementation detail of
lesser importance.

> {-# LANGUAGE OverloadedStrings #-}
> module StateMachinesWithAdts where
>
> import Control.Monad      (foldM)
> import Data.List.NonEmpty
> import Data.Text          (Text)
> import Text.Printf        (printf)
>
> import qualified PaymentProvider
> import           Checkout        ( Card(..)
>                                  , CartItem(..)
>                                  , calculatePrice
>                                  )

`CheckoutState` is a sum type, with one constructor for each valid
state. Some constructors are *nullary*, meaning they have no
arguments. Others have arguments, for the data they carry.

> data CheckoutState
>   = NoItems
>   | HasItems (NonEmpty CartItem)
>   | NoCard (NonEmpty CartItem)
>   | CardSelected (NonEmpty CartItem)
>                  Card
>   | CardConfirmed (NonEmpty CartItem)
>                   Card
>   | OrderPlaced
>   deriving (Show, Eq)

Looking at the state constructors in the definition of
`CheckoutState`, we can see how they accumulate state as the machine
makes progress, right up until the order is placed. Note that
`CartItem` and `Card` are imported from the shared `Checkout` module.

Similar to the data type for states, the data type for events,
called `CheckoutEvent`, defines one constructor for each valid
event. The non-nullary constructors carry some data with the event.

> data CheckoutEvent
>   = Select CartItem
>   | Checkout
>   | SelectCard Card
>   | Confirm
>   | PlaceOrder
>   | Cancel
>   deriving (Show, Eq)

We have now translated the diagram to Haskell data types, and we can
implement the state transitions and actions.

A Pure State Reducer Function
=============================

Now, we might consider the simplest possible implementation of a state
machine a function from state and event to the next state, very much
like the definition from Erlang's documentation quoted above. Such a
function could have the following type:

```{.haskell}
CheckoutState -> CheckoutEvent -> CheckoutState
```

In a state machine that itself can be regarded a pure function, such
as a parser, or a calculator, the above signature would be fine. For
our purposes, however, we need to interleave side effects with state
transitions. We might want to validate that the selected items exist
using external database queries, and send requests to a third-party
payment provider when placing the order.

Reaching for IO
===============

Some systems built around the concept of a state reducer function,
such as [The Elm Architecture][elm-effects] or [Pux][pux], support a
way of specifying the side effects together with the next state. A
starting point to achieve this in Haskell, for our checkout state
machine, is the following type signature:

```{.haskell}
checkout
  :: CheckoutState
  -> CheckoutEvent
  -> IO CheckoutState
```

A state transition then returns `IO` of the next state, meaning that
we can interleave side effects with transitions. We create a type
alias for such a function type, named `FSM`.

> type FSM s e =
>   s -> e -> IO s

Then we can write the type signature for `checkout` using our data
types as parameters.

> checkout :: FSM CheckoutState CheckoutEvent

The definition of `checkout` pattern matches on the current state and
the event. The first five cases simply builds up the state values
based on the events, and transitions appropriately. We could add
validation of selected items, and validation of the selected credit
card, but we would then need explicit error states, or terminate the
entire state machine on such invalid inputs. I'll err on the side of
keeping this example simple.

> checkout NoItems (Select item) =
>   return (HasItems (item :| []))
>
> checkout (HasItems items) (Select item) =
>   return (HasItems (item <| items))
>
> checkout (HasItems items) Checkout =
>   return (NoCard items)
>
> checkout (NoCard items) (SelectCard card) =
>   return (CardSelected items card)
>
> checkout (CardSelected items card) Confirm =
>   return (CardConfirmed items card)

Remember the state diagram? The nested "Checkout" machine accepts the
"cancel" event in all its states, and so does our implementation. We
switch on the current state, and cancel in the correct ones, otherwise
remaining in the current state.

> checkout state Cancel =
>   case state of
>     NoCard items          -> return (HasItems items)
>     CardSelected items _  -> return (HasItems items)
>     CardConfirmed items _ -> return (HasItems items)
>     _                     -> return state

To demonstrate how an interleaved side effect is performed, we use the
imported `chargeCard` and `calculatePrice` to charge the card. The
implementations of `chargeCard` and `calculatePrice` are not
important.

> checkout (CardConfirmed items card) PlaceOrder = do
>   PaymentProvider.chargeCard card (calculatePrice items)
>   return OrderPlaced

The last case is a fall-through pattern, for unaccepted events in the
current state, which effectively has the machine remain in its current
state.

> checkout state _ = return state

That is it for `checkout`, our state reducer function using `IO`.

Running the State Machine
=========================

To run our machine, we can rely on `foldM`. Given a machine, an
initial state, and a foldable sequence of events, we get back the
terminal state inside `IO`.

> runFsm :: Foldable f => FSM s e -> s -> f e -> IO s
> runFsm = foldM

Just getting back the terminal state might be too much of a black
box. To see what happens as we run a machine, we can *decorate* it
with logging. The `withLogging` function runs the state machine it
receives as an argument, logs its transition, and returns the next
state.

> withLogging
>   :: (Show s, Show e)
>   => FSM s e
>   -> FSM s e
> withLogging fsm s e = do
>   s' <- fsm s e
>   printf "- %s × %s → %s\n" (show s) (show e) (show s')
>   return s'

Combining these building blocks and running them in GHCi, with a list
of events as input, we see the transitions logged and our
side-effecting `chargeCard` operation.


```
*StateMachinesWithAdts> runFsm
   (withLogging checkout)
   NoItems
   [ Select (CartItem "potatoes" 23.95)
   , Select (CartItem "fish" 168.50)
   , Checkout
   , SelectCard (Card "0000-0000-0000-0000")
   , Confirm
   , PlaceOrder
   ]
- NoItems × Select (CartItem {itemId = "potatoes", itemPrice = 23.95}) → HasItems (CartItem {itemId = "potatoes", itemPrice = 23.95} :| [])
- HasItems (CartItem {itemId = "potatoes", itemPrice = 23.95} :| []) × Select (CartItem {itemId = "fish", itemPrice = 168.5}) → HasItems (CartItem {itemId = "fish", itemPrice = 168.5} :| [CartItem {itemId = "potatoes", itemPrice = 23.95}])
- HasItems (CartItem {itemId = "fish", itemPrice = 168.5} :| [CartItem {itemId = "potatoes", itemPrice = 23.95}]) × Checkout → NoCard (CartItem {itemId = "fish", itemPrice = 168.5} :| [CartItem {itemId = "potatoes", itemPrice = 23.95}])
- NoCard (CartItem {itemId = "fish", itemPrice = 168.5} :| [CartItem {itemId = "potatoes", itemPrice = 23.95}]) × SelectCard (Card "0000-0000-0000-0000") → CardSelected (CartItem {itemId = "fish", itemPrice = 168.5} :| [CartItem {itemId = "potatoes", itemPrice = 23.95}]) (Card "0000-0000-0000-0000")
- CardSelected (CartItem {itemId = "fish", itemPrice = 168.5} :| [CartItem {itemId = "potatoes", itemPrice = 23.95}]) (Card "0000-0000-0000-0000") × Confirm → CardConfirmed (CartItem {itemId = "fish", itemPrice = 168.5} :| [CartItem {itemId = "potatoes", itemPrice = 23.95}]) (Card "0000-0000-0000-0000")
Charging card 0000-0000-0000-0000 $192.45
- CardConfirmed (CartItem {itemId = "fish", itemPrice = 168.5} :| [CartItem {itemId = "potatoes", itemPrice = 23.95}]) (Card "0000-0000-0000-0000") × PlaceOrder → OrderPlaced
OrderPlaced
```

Yes, the logging is somewhat verbose, but there we have it; a
simplified event-driven state machine using ADTs for states and
events. The data types protect us from constructing illegal values,
they bring the code closer to our conceptual model, and they make
state transitions explicit.

Side Effects and Illegal Transitions
====================================

This is a great starting point, and as I argued in the introduction of
this post, probably the leg on our journey with the highest *return of
investment*. It is, however, still possible to implement illegal state
transitions! We would not get any compile-time error bringing our
attention to such mistakes. Another concern is that the state machine
implementation is tightly coupled with IO, making it hard to test.

We could factor out the side effects in `checkout` using [MTL-style
typeclasses or free
monads](https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html),
, or perhaps using
[extensible-effects](https://hackage.haskell.org/package/extensible-effects-1.11.1.0). That
said, in the next post I will show you a technique to tackle both the
side effect and testability concerns, using MTL-style abstraction of
the state machine itself. Stay tuned!

[elm-effects]: https://guide.elm-lang.org/architecture/effects/
[pux]: http://purescript-pux.org/docs/events/#Effectful_computations
