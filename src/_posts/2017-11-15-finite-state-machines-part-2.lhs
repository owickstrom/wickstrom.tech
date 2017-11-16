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
important. The `Prompt` module provides helpers for retrieving input from
the terminal.

> import qualified PaymentProvider
> import           Checkout        ( Card(..)
>                                  , CartItem(..)
>                                  , OrderId(..)
>                                  , mkItem
>                                  , calculatePrice
>                                  , newOrderId
>                                  )
> import           Prompt          ( prompt
>                                  , confirmPrompt
>                                  )

Enough imports, let's go build our state machine!

The State Machine Protocol
==========================

> data NoItems
> data HasItems
> data NoCard
> data CardSelected
> data CardConfirmed
> data OrderPlaced

> data SelectState m
>   = NoItemsSelect (State m NoItems)
>   | HasItemsSelect (State m HasItems)
>
> data CancelState m
>   = NoCardCancel (State m NoCard)
>   | CardSelectedCancel (State m CardSelected)
>   | CardConfirmedCancel (State m CardConfirmed)

> class Checkout m where
>   type State m :: * -> *
>   initial :: m (State m NoItems)
>   select ::
>        SelectState m
>     -> CartItem
>     -> m (State m HasItems)
>   checkout :: State m HasItems -> m (State m NoCard)
>   selectCard ::
>        State m NoCard -> Card -> m (State m CardSelected)
>   confirm ::
>        State m CardSelected -> m (State m CardConfirmed)
>   placeOrder ::
>        State m CardConfirmed -> m (State m OrderPlaced)
>   cancel :: CancelState m -> m (State m HasItems)
>   end :: State m OrderPlaced -> m OrderId

> fillCart ::
>      (Checkout m, MonadIO m)
>   => State m NoItems
>   -> m (State m HasItems)
> fillCart noItems =
>   mkItem <$> prompt "First item:"
>   >>= select (NoItemsSelect noItems)
>   >>= selectMoreItems

> selectMoreItems ::
>      (Checkout m, MonadIO m)
>   => State m HasItems
>   -> m (State m HasItems)
> selectMoreItems s = do
>   more <- confirmPrompt "More items?"
>   if more
>     then
>       mkItem <$> prompt "Next item:"
>       >>= select (HasItemsSelect s)
>       >>= selectMoreItems
>     else return s

> startCheckout ::
>      (Checkout m, MonadIO m)
>   => State m HasItems
>   -> m (State m OrderPlaced)
> startCheckout hasItems = do
>   noCard <- checkout hasItems
>   card <- prompt "Card:"
>   cardSelected <- selectCard noCard (Card card)
>   useCard <- confirmPrompt ("Confirm use of '" <> card <> "'?")
>   if useCard
>     then confirm cardSelected >>= placeOrder
>     else cancel (CardSelectedCancel cardSelected) >>=
>          selectMoreItems >>=
>          startCheckout

> checkoutProgram ::
>      (Checkout m, MonadIO m)
>   => m OrderId
> checkoutProgram =
>   initial >>= fillCart >>= startCheckout >>= end

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
<strong>1290312093213</strong>
Confirm use of '1290312093213'? (y/N)
<strong>y</strong>
Charging card 1290312093213 $200
Completed with order ID: foo
</pre>
