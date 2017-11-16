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

![](/generated/uml/checkout.svg)

> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE GADTs                      #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE TypeFamilies               #-}
> module EnforcingLegalStateTransision where
>
> import           Control.Monad.IO.Class
> import           Data.List.NonEmpty
> import           Data.Semigroup
> import qualified Data.Text.IO             as T
>
> import qualified PaymentProvider
> import           Checkout        ( Card(..)
>                                  , CartItem(..)
>                                  , OrderId(..)
>                                  , mkItem
>                                  , calculatePrice
>                                  , newOrderId
>                                  )
> import           Prompt

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
