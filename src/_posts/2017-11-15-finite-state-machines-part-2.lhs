---
layout: post
title: "Finite-State Machines, Part 2: Explicit Typed State Transitions"
date: 2017-11-15 06:00 +01:00
author: Oskar Wickström
categories: finite-state-machines
tags: ["haskell", "functional", "type-systems"]
published: false
excerpt: |
  TODO!
---


In [the first part of this
series](/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html),
we left off with having made states explicit using Haskell data
types. We concluded that state transitions were implicit, and that a
mistake in implementation, making an erroneous state transition, would
not be caught by the type system. We also noted that side effects
performed at state transitions complicated testing of the state
machine, as we were tied to `IO`.

Before addressing those problems, let's remind ourselves of the
example state machine diagram. If you haven't read the previous post,
I recommend you go do that first.

![](/generated/uml/checkout.svg)

As before, this post is a runnable Literate Haskell program. We begin
with the language extensions we'll need, along with the module
declaration.

> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE GADTs                      #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE TypeFamilies               #-}
> module EnforcingLegalStateTransision where

Let me quickly explain the GHC language extensions used:

* `OverloadedStrings` converts string literals in Haskell source code
  to `Text` values, in our case.
* `GADTs` enables the use of *generalized algebraic data types*, an extension to
  GHC that lets us specify different type signatures for each constructor in
  data type. This is useful to parameterize constructors with
  different types, something we'll use for state data types.
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
post](http://localhost:4000/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html#finite-state-machines),
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
state type it is parameterized by. Therefore we use an associated type
alias, also known as an open type family, with kind `* -> *` to
represent states.

>   type State m :: * -> *

The signature `* -> *` can be thought of as a type-level function, or
a type constructor, from type to type (the star is the kind of types).
The parameter `m` to state means we are associating the state type
with the instance of `m`, so that different instances can specify
their own concrete state types.

In our case, the parameter will always be one of our empty data types
declared for states. As an example, `(State m NoItems)` has kind `*`,
and is used to represent the "NoItems" state abstractly.

Note that `m` *also* has kind `* -> *`, but not for the same reason;
the `m` is going to be the monadic type we use for our implementation,
and is therefore *higher-kinded* as well.

Events as Type Class Methods
----------------------------

`Checkout` specifies the state machine events as type class *methods*,
where method type signatures describe state transitions. The `initial`
method creates a new checkout, returning a "NoItems" state. It can be
thought of as a *constructor*, in object-oriented programming terms.

>   initial
>     :: m (State m NoItems)

The value returned, of type `(State m NoItems)`, is the first state.
We use this value as a parameter to the subsequent event,
transitioning to another state. Events that transition state from one
to another take the current state as an argument, and return the
resulting state.

The `select` event is a bit tricky, as it accepted from both "NoItems"
and "HasItems". We use the union data type `SelectState`, analogous to
`Either`, that represents the possibility of either "NoItems" or
"HasItems". The definition of `SelectState` is included further down
this post.

>   select
>     :: SelectState m
>     -> CartItem
>     -> m (State m HasItems)

The `checkout` event is simpler than `select`, as it transitions from
exactly one state to another.

Worth noting is that the resulting state is returned inside `m`. We do
that to enable the instance of `Checkout` to perform computations
available in `m` at the state transition.

*Does this ring a bell?*

Just as in the previous post, we want the possibility to interleave
side effects on state transitions, and using a monadic return value
gives the instance that flexibility.

Some events, like `selectCard`, carry data in the form of arguments,
corresponding to how some event data constructors had arguments. Most
of the events in `Checkout` follow the patterns described so far.

>   checkout
>     :: State m HasItems
>     -> m (State m NoCard)
>
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

Similarly to `select`, the `cancel` event is accepted from more than
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
`CancelState`, the data types that represent alterative source states
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
"NoItems" state to a "HasItems" state. The type is similiar to the
event methods' type signatures in the `Checkout` protocol, and
similarly describes a state transition with a type.

> fillCart ::
>      (Checkout m, MonadIO m)
>   => State m NoItems
>   -> m (State m HasItems)

This is where we are starting to use the [MTL
style](https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html)
of abstracting effects, and combining different effects by
constraining the monadic type with multiple type classes.

The critical reader might object to using `MonadIO`, and claim that we
have not separated all side effects, and failed in making the program
testable. They wouldn't be wrong. I have deliberately kept the direct
use of `MonadIO` to keep the example somewhat concrete. We could
refactor it to depend on, say, a `UserInput` type class for collecting
more abtract user commands. By using `MonadIO`, though, the example
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

> selectMoreItems ::
>      (Checkout m, MonadIO m)
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
part. The type signature of `startCheckout` tells us that is
transitions from a "HasItems" state to an "OrderPlaced" state.

> startCheckout ::
>      (Checkout m, MonadIO m)
>   => State m HasItems
>   -> m (State m OrderPlaced)

The function starts the checkout, prompts for a card, and selects
it. It asks the user to confirm the use of the selected card, and ends
by placing the order. If the user did not confirm, the checkout is
cancelled, and we go back to selecting more items, followed by
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

Note that the protocol allows for cancellation in all three checkout
states, but that the program only gives the user a possibility to
cancel in the end of the process. Again, the program must follow the
rules of the protocol, but it is not required to trigger all events
the protocol allows for.

The definition of `checkoutProgram` is a composition of what we have
so far. It creates the state machine in its initial state, fills the
shopping cart, starts the checkout, and eventually ends the checkout.

> checkoutProgram ::
>      (Checkout m, MonadIO m)
>   => m OrderId
> checkoutProgram =
>   initial >>= fillCart >>= startCheckout >>= end

We now have a complete, albeit simple, program using the `Checkout`
state machine protocol. All we need now is an instance of `Checkout`.

Defining an Instance for Checkout
=================================

> newtype CheckoutT m a = CheckoutT
>   { runCheckoutT :: m a
>   } deriving ( Monad
>              , Functor
>              , Applicative
>              , MonadIO
>              )

> data CheckoutState s where
>   NoItems
>     :: CheckoutState NoItems
>
>   HasItems
>     :: NonEmpty CartItem -> CheckoutState HasItems
>
>   NoCard
>     :: NonEmpty CartItem -> CheckoutState NoCard
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
>   OrderPlaced :: OrderId -> CheckoutState OrderPlaced

> instance (MonadIO m) => Checkout (CheckoutT m) where
>   type State (CheckoutT m) = CheckoutState
>   initial = return NoItems
>   select state item =
>     case state of
>       NoItemsSelect NoItems ->
>         return (HasItems (item :| []))
>       HasItemsSelect (HasItems items) ->
>         return (HasItems (item <| items))
>   checkout (HasItems items) = return (NoCard items)
>   selectCard (NoCard items) card =
>     return (CardSelected items card)
>   confirm (CardSelected items card) =
>     return (CardConfirmed items card)
>   placeOrder (CardConfirmed items card) = do
>     orderId <- newOrderId
>     let price = calculatePrice items
>     PaymentProvider.chargeCard card price
>     return (OrderPlaced orderId)
>   cancel cancelState =
>     case cancelState of
>       NoCardCancel (NoCard items) -> return (HasItems items)
>       CardSelectedCancel (CardSelected items _) ->
>         return (HasItems items)
>       CardConfirmedCancel (CardConfirmed items _) ->
>         return (HasItems items)
>   end (OrderPlaced orderId) = return orderId

Putting the Pieces Together
===========================

> example :: IO ()
> example = do
>   OrderId orderId <- runCheckoutT checkoutProgram
>   T.putStrLn ("Completed with order ID: " <> orderId)

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
