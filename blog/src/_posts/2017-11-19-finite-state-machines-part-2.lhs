---
layout: post
title: "Finite-State Machines, Part 2: Explicit Typed State Transitions"
date: 2017-11-19 06:00 +01:00
author: Oskar Wickström
categories: finite-state-machines
tags: ["haskell", "functional", "type-systems"]
excerpt: |
  In the second post of the Finite-State Machines series, we improve
  type safety around state transitions and their side effects, and
  make testing state machines without side effects easier, using an
  extended MTL style encoding.
---


In [the first part of this
series](/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html),
we left off having made states explicit using Haskell data types. We
concluded that state transitions were implicit, and that a mistake in
implementation, making an erroneous state transition, would not be
caught by the type system. We also noted that side effects performed
at state transitions complicated testing of the state machine, as we
were tied to `IO`.

Before addressing those problems, let's remind ourselves of the
example state machine diagram. If you haven't read the previous post,
I recommend you go do that first.

![](/generated/uml/checkout.svg)

Again, the post is a runnable Literate Haskell program. We begin with
the language extensions we'll need, along with the module declaration.

> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE GADTs                      #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE TypeFamilies               #-}
> module EnforcingLegalStateTransitions where

Let me quickly explain the GHC language extensions used:

* `OverloadedStrings` converts string literals in Haskell source code
  to `Text` values, in our case.
* `GADTs` enables the use of *generalized algebraic data types*, an extension to
  GHC that lets us specify different type signatures for each constructor in
  a data type. This is useful to parameterize constructors with
  different types, something we will use for state data types.
* We use `GeneralizedNewtypeDeriving` to have our implementation
  newtype derive instances for `Functor`, `Applicative`, and
  `MonadIO`. I'll explain this later in this post.
* `TypeFamilies` extends GHC Haskell with what you can consider
  type-level functions. We'll use this extension to associate a concrete state
  data type with our instance of the state machine.

In addition to `Control.Monad.IO.Class`, we import the same modules as
in the previous post.

> import           Control.Monad.IO.Class
> import           Data.List.NonEmpty
> import           Data.Semigroup
> import qualified Data.Text.IO             as T

From the modules specific to the blog post series we import some
functions and data types. As before, their exact implementations are not
important. The `ConsoleInput` module provides helpers for retrieving
text input and confirmation from the terminal.

> import qualified PaymentProvider
> import           Checkout        ( Card(..)
>                                  , CartItem
>                                  , OrderId(..)
>                                  , mkItem
>                                  , calculatePrice
>                                  , newOrderId
>                                  )
> import qualified ConsoleInput

Enough imports, let's go build our state machine!

The State Machine Protocol
==========================

In contrast to the transition function in [the previous
post](/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html#finite-state-machines),
where a single function had the responsibility of deciding which state
transitions were legal, performing associated side effects on
transitions, and transitioning to the next state, we will now separate
the _protocol_ from the _program_. In other words, the set of states,
and the associated state transitions for certain events, will be
encoded separately from the implementation of the
automaton. Conceptually, it still follows the definition we borrowed
from Erlang:

<blockquote>
<p>*State(S) &times; Event(E) &rarr; Actions (A), State(S')*</p>
</blockquote>

With the risk of stretching a metaphor too thin, I like to think of
the split representation as taking the single-function approach and
turning it inside-out. Our program is separate from our state machine
protocol, and we can implement it however we like, as long as we
follow the protocol.

Empty Data Types for States
---------------------------

Our states are no longer represented by a single data type with
constructors for each state. Instead, we create an _empty data type_
for each state. Such a type has no constructors, and is therefore not
inhabited by any value. We will use them solely as *markers* in GADT
constructors, a technique in general known as [phantom
types](https://stackoverflow.com/a/28250226).

> data NoItems
> data HasItems
> data NoCard
> data CardSelected
> data CardConfirmed
> data OrderPlaced

I know that phantom types and GADTs sound scary at first, but please
hang in there, as I'll explain their practical use throughout this
post, and hopefully give you a sense of why we are using them.

State Machine Protocol as a Type Class
--------------------------------------

We encode our state machine protocol, separate from the program, using
a *type class*.

> class Checkout m where

In our protocol, we do not want to be specific about what data
type is used to represent the current state; we only care about the
state type it is parameterized by. Therefore, we use an associated type
alias, also known as an open type family, with kind `* -> *` to
represent states.

>   type State m :: * -> *

The signature `* -> *` can be thought of as a type-level function, or
a type constructor, from type to type (the star is the kind of types).
The parameter `m` to state means we are associating the state type
with the instance of `m`, so that different instances can specify
their own concrete state types.

There are two benefits of using an associated type for state:

1. Different instances of `Checkout` can provide different concrete
  state data types, hiding whatever nasty implementation details they
  need to operate, such as database connections, web sessions, or file
  handles.
2. The concrete state type is not known when using the state machine
  protocol, and it is therefore impossible to create a state
  "manually"; the program would not typecheck.

In our case, the parameter will always be one of our empty data types
declared for states. As an example, `(State m NoItems)` has kind `*`,
and is used to represent the "NoItems" state abstractly.

Note that `m` *also* has kind `* -> *`, but not for the same reason;
the `m` is going to be the monadic type we use for our implementation,
and is therefore higher-kinded.

Events as Type Class Methods
----------------------------

`Checkout` specifies the state machine events as type class *methods*,
where method type signatures describe state transitions. The `initial`
method creates a new checkout, returning a "NoItems" state. It can be
thought of as a *constructor* in object-oriented programming terms.

>   initial
>     :: m (State m NoItems)

The value returned, of type `(State m NoItems)`, is the first state.
We use this value as a parameter to the subsequent event,
transitioning to another state. Events that transition state from one
to another take the current state as an argument, and return the
resulting state.

The `select` event is a bit tricky, as it is accepted from both
"NoItems" and "HasItems". We use the union data type `SelectState`,
analogous to `Either`, that represents the possibility of either
"NoItems" or "HasItems". The definition of `SelectState` is included
further down this post.

>   select
>     :: SelectState m
>     -> CartItem
>     -> m (State m HasItems)

Worth noting is that the resulting state is returned inside `m`. We do
that to enable the instance of `Checkout` to perform computations
available in `m` at the state transition.

*Does this ring a bell?*

Just as in the previous post, we want the possibility to interleave
side effects on state transitions, and using a monadic return value
gives the instance that flexibility.

The `checkout` event is simpler than `select`, as it transitions from
exactly one state to another.

>   checkout
>     :: State m HasItems
>     -> m (State m NoCard)

Some events, like `selectCard`, carry data in the form of arguments,
corresponding to how some event data constructors had arguments. Most
of the events in `Checkout` follow the patterns described so far.

>   selectCard
>     :: State m NoCard
>     -> Card
>     -> m (State m CardSelected)
>
>   confirm
>      :: State m CardSelected
>      -> m (State m CardConfirmed)
>
>   placeOrder
>     :: State m CardConfirmed
>     -> m (State m OrderPlaced)


Note that we are *not doing any error handling*. All operations return
state values. In a real-world system you might need to handle error
cases, like `selectCard` not accepting the entered card number. I have
deliberately excluded error handling from this already lengthy post,
but I will probably write a post about different ways of handling
errors in this style of state machine encoding.


Similar to `select`, the `cancel` event is accepted from more than
one state. In fact, it is accepted from *three* states: "NoCard",
"CardSelected", and "CardConfirmed". Like with `select`, we use a
union data type representating the ternary alternative.

>   cancel
>     :: CancelState m
>     -> m (State m HasItems)

Finally, we have the `end` method as a way of ending the state machine
in its terminal state, similar to a *destructor* in object-oriented
programming terms. Instances of `Checkout` can have `end` clean up
resources associated with the machine.

>   end
>     :: State m OrderPlaced
>     -> m OrderId

As promised, I will show you the definitions of `SelectState` and
`CancelState`, the data types that represent alternative source states
for the `select` and `cancel` events, respectively.

> data SelectState m
>   = NoItemsSelect (State m NoItems)
>   | HasItemsSelect (State m HasItems)
>
> data CancelState m
>   = NoCardCancel (State m NoCard)
>   | CardSelectedCancel (State m CardSelected)
>   | CardConfirmedCancel (State m CardConfirmed)

Each constructor takes a specific state as argument, thus creating a
union type wrapping the alternatives.

A Program using the State Machine Protocol
==========================================

Now that we have a state machine protocol, the `Checkout` type class,
we can write a program with it. This is the automaton part of our
implementation, i.e. the part that *drives* the state machine forward.

As long as the program follows the protocol, it can be structured
however we like; we can drive it using user input from a console, by
listening to a queue of commands, or by incoming HTTP requests from a
web server. In the interest of this post, however, we will keep to
reading user input from the console.

The type signature of `fillCart` constrains `m` to be an instance of
both `Checkout` and `MonadIO`. Moreover, it is a function from a
"NoItems" state to a "HasItems" state. The type is similar to the
event methods' type signatures in the `Checkout` protocol, and
similarly describes a state transition with a type.

> fillCart
>   :: (Checkout m, MonadIO m)
>   => State m NoItems
>   -> m (State m HasItems)

This is where we are starting to use the [MTL
style](https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html)
of abstracting effects, and combining different effects by
constraining the monadic type with multiple type classes.

The critical reader might object to using `MonadIO`, and claim that we
have not separated all side effects, and failed in making the program
testable. They wouldn't be wrong. I have deliberately left the direct
use of `MonadIO` in to keep the example somewhat concrete. We could
refactor it to depend on, say, a `UserInput` type class for collecting
more abstract user commands. By using `MonadIO`, though, the example
highlights particularly how the state machine protocol has been
abstracted, and how the effects of state transitions are guarded by
the type system, rather than making everything abstract. I encourage
you to try out both approaches in your code!

The definition of `fillCart` takes a "NoItems" state value as an
argument, prompts the user for the first cart item, selects it, and
hands off to `selectMoreItems`.

> fillCart noItems =
>   mkItem <$> ConsoleInput.prompt "First item:"
>   >>= select (NoItemsSelect noItems)
>   >>= selectMoreItems

The event methods of the `Checkout` protocol,
and functions like `fillCart` and `selectMoreItems`, are functions
from one state to a monadic return value of another state, and thus
compose using `(>>=)`.

The `selectMoreItems` function remains in a "HasItems" state. It asks
the user if they want to add more items. If so, it asks for the next
item, selects that and recurses to possibly add even more items; if
not, it returns the current "HasItems" state. Note how we need to wrap
the "HasItems" state in `HasItemsSelect` to create a `SelectState`
value.

> selectMoreItems
>   :: (Checkout m, MonadIO m)
>   => State m HasItems
>   -> m (State m HasItems)
> selectMoreItems s = do
>   more <- ConsoleInput.confirm "More items?"
>   if more
>     then
>       mkItem <$> ConsoleInput.prompt "Next item:"
>       >>= select (HasItemsSelect s)
>       >>= selectMoreItems
>     else return s

When all items have been added, we are ready to start the checkout
part. The type signature of `startCheckout` tells us that it
transitions from a "HasItems" state to an "OrderPlaced" state.

> startCheckout
>   :: (Checkout m, MonadIO m)
>   => State m HasItems
>   -> m (State m OrderPlaced)

The function starts the checkout, prompts for a card, and selects
it. It asks the user to confirm the use of the selected card, and ends
by placing the order. If the user did not confirm, the checkout is
canceled, and we go back to selecting more items, followed by
attempting a new checkout.

> startCheckout hasItems = do
>   noCard <- checkout hasItems
>   card <- ConsoleInput.prompt "Card:"
>   cardSelected <- selectCard noCard (Card card)
>   useCard <- ConsoleInput.confirm ("Confirm use of '" <> card <> "'?")
>   if useCard
>     then confirm cardSelected >>= placeOrder
>     else cancel (CardSelectedCancel cardSelected) >>=
>          selectMoreItems >>=
>          startCheckout

The protocol allows for cancellation in all three checkout
states, but that the program only gives the user a possibility to
cancel in the end of the process. Again, the program must follow the
rules of the protocol, but it is not required to trigger all events
the protocol allows for.

The definition of `checkoutProgram` is a composition of what we have
so far. It creates the state machine in its initial state, fills the
shopping cart, starts the checkout, and eventually ends the checkout.

> checkoutProgram
>   :: (Checkout m, MonadIO m)
>   => m OrderId
> checkoutProgram =
>   initial >>= fillCart >>= startCheckout >>= end

We now have a complete program using the `Checkout` state machine
protocol. To run it, however, we need an instance of `Checkout`.

Defining an Instance for Checkout
=================================

To define an instance for `Checkout`, we need a type to define it
for. A common way of defining such types, especially in MTL style, is
using `newtype` around a monadic value. The type name often ends with
`T` to denote that it's a transformer. Also by convention, the
constructor takes a single-field record, where the field accessor
follows the naming scheme `run<TypeName>`; in our case `runCheckoutT`.

> newtype CheckoutT m a = CheckoutT
>   { runCheckoutT :: m a
>   } deriving ( Functor
>              , Monad
>              , Applicative
>              , MonadIO
>              )

We derive the `MonadIO` instance automatically, along with the
standard `Functor`, `Applicative`, and `Monad` hierarchy.

Monad Transformer Stacks and Instance Coupling
----------------------------------------------

Had we not derived `MonadIO`, the program from before, with
constraints on both `Checkout` and `MonadIO`, would not have
compiled. Therein lies a subtle dependency that is hard to see at
first, but that might cause you a lot of headache. Data types used to
instantiate MTL style type classes, when stacked, need to implement
all type classes in use. This is caused by the *stacking* aspect of
monad transformers, and is a common critique of MTL style.

Other techniques for separating side effects, such as free monads or
extensible effects, have other tradeoffs. I have chosen to focus on
MTL style as it is widely used, and in my opinion, a decent starting
point. If anyone rewrites these examples using another technique,
please drop a comment!

A Concrete State Data Type
--------------------------

Remember how we have, so far, only been talking about state values
abstractly, in terms of the associated type alias `State` in the
`Checkout` class? It is time to provide the concrete data type for
state that we will use in our instance.

As discussed earlier, the type we associate for `State` need to have
kind `(* -> *)`. The argument is the state marker type, i.e. one of the
empty data types for states. We define the data type `CheckoutState`
using a GADT, where `s` is the state type.

> data CheckoutState s where

With `GADTs`, data constructors specify their own type signatures,
allowing the use of phantom types, and differently typed values
resulting from the constructors. Each constructor parameterize the
`CheckoutState` with a different state type.

The `NoItems` constructor is nullary, and constructs a value of type
`CheckoutState NoItems`.

>   NoItems
>     :: CheckoutState NoItems

<div class="note">
The *data constructor* `NoItems` is defined here, whereas the *type*
`NoItems` is defined in the beginning of the program, and they are
*not directly related*.
</div>

There is, however, a relation between them in terms of the
`CheckoutState` data type. If we have a value of type `CheckoutState
NoItems`, and we pattern match on it, GHC knows that there is only one
constructor for such a value. This will become very handy when
defining our instance.

The other constructors are defined similarly, but some have arguments,
in the same way the `State` data type in the previous post had. They
accumulate the extended state needed by the state machine, up until
the order is placed.

>   HasItems
>     :: NonEmpty CartItem
>     -> CheckoutState HasItems
>
>   NoCard
>     :: NonEmpty CartItem
>     -> CheckoutState NoCard
>
>   CardSelected
>     :: NonEmpty CartItem
>     -> Card
>     -> CheckoutState CardSelected
>
>   CardConfirmed
>     :: NonEmpty CartItem
>     -> Card
>     -> CheckoutState CardConfirmed
>
>   OrderPlaced
>     :: OrderId
>     -> CheckoutState OrderPlaced

We have a concrete state data type, defined as a GADT, and we can go
ahead defining the instance of `Checkout` for our `CheckoutT` newtype.
We need `MonadIO` to perform `IO` on state transitions, such as
charging the customer card.

> instance (MonadIO m) => Checkout (CheckoutT m) where

Next, we can finally tie the knot, associating the state type for
`CheckoutT` with `CheckoutState`.

>   type State (CheckoutT m) = CheckoutState

We continue by defining the methods. The `initial` method creates
the state machine by returning the initial state, the `NoItems`
constructor.

>   initial = return NoItems

In `select`, we receive the current state, which can be either one
of the constructors of `SelectState`. Unwrapping those gives us the
`CheckoutState` value. We return the `HasItems` state with the selected
item prepended to a non-empty list.

>   select state item =
>     case state of
>       NoItemsSelect NoItems ->
>         return (HasItems (item :| []))
>       HasItemsSelect (HasItems items) ->
>         return (HasItems (item <| items))

As emphasized before, GHC knows which constructors of `CheckoutState`
can occur in the `SelectState` wrappers, and we can pattern match
exhaustively on only the possible state constructors.

The `checkout`, `selectCard`, and `confirm` methods accumulate the
extended state, and returns the appropriate state constructor.

>   checkout (HasItems items) =
>     return (NoCard items)
>
>   selectCard (NoCard items) card =
>     return (CardSelected items card)
>
>   confirm (CardSelected items card) =
>     return (CardConfirmed items card)

Now for `placeOrder`, where we want to perform a side effect. We have
constrained `m` to be an instance of `MonadIO`, and we can thus use
the effectful `newOrderId` and `PaymentProvider.chargeCard` in our
definition.

>   placeOrder (CardConfirmed items card) = do
>     orderId <- newOrderId
>     let price = calculatePrice items
>     PaymentProvider.chargeCard card price
>     return (OrderPlaced orderId)

Similar to `select`, `cancel` switches on the alternatives of the
`CancelState` data type. In all cases it returns the `HasItems` state
with the current list of items.

>   cancel cancelState =
>     case cancelState of
>       NoCardCancel (NoCard items) ->
>         return (HasItems items)
>       CardSelectedCancel (CardSelected items _) ->
>         return (HasItems items)
>       CardConfirmedCancel (CardConfirmed items _) ->
>         return (HasItems items)

Finally, the definition of `end` returns the generated order
identifier.

>   end (OrderPlaced orderId) = return orderId

The `CheckoutT` instance of `Checkout` is complete, and we are ready
to stitch everything together into a running program.

Putting the Pieces Together
===========================

To run `checkoutProgram`, we need an instance of `Checkout`, and an
instance of `MonadIO`. There is already an instance `(MonadIO IO)`
available. To select our `CheckoutT` instance for `Checkout`, we use
`runCheckoutT`.

> example :: IO ()
> example = do
>   OrderId orderId <- runCheckoutT checkoutProgram
>   T.putStrLn ("Completed with order ID: " <> orderId)

The complete checkout program is run, using the `CheckoutT` instance,
and an `OrderId` is returned, which we print at the end. A sample
execution of this program looks like this:

<pre>
λ> <strong>example</strong>
First item:
<strong>Banana</strong>
More items? (y/N)
<strong>y</strong>
Next item:
<strong>Horse</strong>
More items? (y/N)
<strong>y</strong>
Next item:
<strong>House</strong>
More items? (y/N)
<strong>n</strong>
Card:
<strong>0000-0000-0000-0000</strong>
Confirm use of '0000-0000-0000-0000'? (y/N)
<strong>y</strong>
Charging card 0000-0000-0000-0000 $200
Completed with order ID: foo
</pre>

Cool, we have a console implementation running!

Instances Without Side Effects
------------------------------

A benefit of using MTL style, in addition to have effects be explicit,
is that we can write alternative instances. We might write an instance
that only logs the effects, using a `Writer` monad , collecting them
as pure values in a list, and use that instance when testing the state
machine.

Parting Thoughts
================

Using a sort of *extended MTL style*, with conventions for state
machine encodings, gives us more type safety in state transitions. In
addition to having turned our state machine program *inside-out*, into
a protocol separated from the automaton, we have guarded side effects
with types in the form of type class methods.  Abstract state values,
impossible to create outside the instance, are now passed explicitly
in state transitions.

But we still have a rather loud elephant in the room.

Suppose I'd write the following function, wherein I place the order
*twice*. Do you think it would typecheck?

```{.haskell}
doBadThings ::
     (Checkout m, MonadIO m)
  => State m CardConfirmed
  -> m (State m OrderPlaced)
doBadThings cardConfirmed = do
  _ <- placeOrder cardConfirmed
  placeOrder cardConfirmed
```

The answer is yes, it would typecheck. With the `Checkout` instance we
have, the customer's card would be *charged twice*, without doubt
departing from our business model, and likely hurting our brand.

The problem is that we are allowed to discard the state transitioned
to, a value of type `(State m OrderPlaced)`, returned by the first
`placeOrder` expression. Then, we can place the order again, using the
old state value of type `(State m CardConfirmed)`. The ability to
reuse, or never use, state values is the *Achilles' heel* of this
post's state machine encoding.

We could venture into the land of *linear types*, a feature [recently
proposed to be added to
GHC](https://github.com/ghc-proposals/ghc-proposals/pull/91/files). With
linear types, we could ensure state values are used *exactly once*,
making our current approach safer.

I'd like to step back for a moment, however, and remind you that the
techniques we encounter along this journey are not ordered as
increasingly "better", in terms of what you should apply in your
work. I show more and more advanced encodings, using various GHC
language extensions, but it doesn't mean you should necessarily use
the most advanced one. Simplicity is powerful, something [Tim
Humphries tweet thread reminded me about this
morning](https://twitter.com/thumphriees/status/932137942222385153),
and I recommend you start out simple.

As demonstrated, the extended MTL style for encoding state machines
presented in this post has a type safety flaw. That doesn't mean the
technique is useless and should be forever rejected. At least not in
my opinion. It gives additional type safety around state transitions,
it composes well with MTL style programs in general, and it uses a
modest collection of type system features and language extensions. We
can write alternative instances, without any IO, and use them to test
our state machines programs in a pure setting.

If you still feel that all hope is lost, then I'm happy to announce
that there will be more posts coming in this series! To demonstrate a
possible next step, in terms of even greater type safety, in the next
post we will be exploring *indexed monads* and *row kinds* as a way of
armoring the Achilles heel.

Happy hacking!

Revisions
=========

**November 20, 2017:** Based on a Reddit comment, on the lack of error
handling in event type class methods, I added a small note about that
below the `selectCard` type signature.
