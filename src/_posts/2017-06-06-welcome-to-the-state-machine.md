---
layout: post
title: Welcome to the State Machine
author: Oskar Wickström
categories: programming
tags: ["haskell", "functional", "dsl"]
excerpt: "Todo..."
---

``` haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module WelcomeToTheStateMachine where

import Prelude hiding (getLine, putStrLn, head, (!!))
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty(..), (!!))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as IO
import Text.Printf

data CartItem =
  CartItem Text
  deriving (Show, Eq)

data ItemsSelected

data Confirmed

newtype Card =
  Card Text
  deriving (Show, Eq)

newtype Coupon =
  Coupon Text
  deriving (Show, Eq)

newtype Discount =
  Discount Double -- discount percentage
  deriving (Show, Eq)
```

``` haskell
class Checkout m where
  type State m :: * -> *
  select :: [CartItem] -> m [CartItem]
  getCoupon :: m (Maybe Coupon)
  getCards :: m [Card]
  selectCard :: NonEmpty Card -> m Card
  confirm :: Card
          -> [CartItem]
          -> Price
          -> m (Maybe (State m Confirmed))
```

``` haskell
class Coupons m where
  lookupDiscount :: Coupon -> m (Maybe Discount)
```

``` haskell
newtype Price =
  Price Double
  deriving (Show, Eq)
```

``` haskell
class Payments m where
  pay :: Card -> Price -> m Bool
```

``` haskell
data CheckoutError
  = NoCardAvailable
  | InvalidCoupon Coupon
  | NotConfirmed
  deriving (Eq, Show)
```

The `getDiscount` function tries to get a discount, given an optional coupon. If a coupon is given, but no discount exist for it, an `InvalidCoupon` error is thrown.

``` haskell
getDiscount
  :: (Monad m, Coupons m, MonadError CheckoutError m)
  => Maybe Coupon -> m (Maybe Discount)
getDiscount = maybe (return Nothing) lookup
  where
    lookup coupon =
      lookupDiscount coupon >>=
      maybe (throwError (InvalidCoupon coupon))
            (return . Just)
```

``` haskell
checkout ::
     ( Checkout m
     , Coupons m
     , Payments m
     , MonadError CheckoutError m
     )
  => [CartItem]
  -> m Bool
checkout items = do
  selected <- select items
  discount <- getCoupon >>= getDiscount
  card <-
    getCards >>= \case
      [] -> throwError NoCardAvailable
      card:cards -> selectCard (card :| cards)
  let price = (calculatePrice items discount)
  confirm card selected price
  pay card price
  where
    calculatePrice items discount = Price 0
```

Interpreter
-----------

``` haskell
newtype CheckoutT m a =
  CheckoutT (m a)
  deriving ( Monad
           , Functor
           , Applicative
           , MonadError e
           , MonadIO
           , Payments
           , Coupons
           )

runCheckoutT :: MonadIO m => CheckoutT m a -> m a
runCheckoutT (CheckoutT a) = a

data Payment s where
  ItemsSelected :: Payment ItemsSelected
```

``` haskell
readConfirmation :: MonadIO m => Text -> m Bool
readConfirmation t = liftIO $ do
  printf "%s (Y/n)\n" t
  s <- T.toLower . T.strip <$> getLine
  return (s == "y")
```

``` haskell
data CheckoutState s where
  Confirmed :: CheckoutState Confirmed
```

``` haskell
instance MonadIO m => Checkout (CheckoutT m) where
  type State (CheckoutT m) = CheckoutState

  select items = do
    filterM confirmItem items
    where
      confirmItem (CartItem item) =
        readConfirmation (T.pack (printf "Do you want to buy '%s'?" item))
  getCoupon = liftIO $ do
    putStrLn "Enter a coupon, if you have one:"
    s <- T.strip <$> getLine
    if T.null s
      then return Nothing
      else return (Just (Coupon s))
  getCards = liftIO $ do
    return [Card "0000 0000 0000 0000"]
  selectCard cards = do
    chosen <- liftIO chooseCard
    card@(Card cardNr) <- maybe (selectCard cards) return chosen
    liftIO (printf "You've chosen the card '%s'.\n\n" cardNr)
    return card
    where
      printChoice :: Card -> Int -> IO ()
      printChoice (Card c) n = printf "%d: %s\n" n c
      chooseCard = do
        putStrLn "Which card do you want to pay with? (Enter a number)"
        zipWithM_ printChoice (NonEmpty.toList cards) [1..]
        n <- readLn
        putStrLn ""
        if n > 0 && n <= NonEmpty.length cards
          then return (Just (cards !! (n - 1)))
          else chooseCard

  confirm (Card card) items (Price price) = liftIO $ do
    printf "Number of selected items: %d\n" (length items)
    printf "Price: %d\n" price
    printf "Selected card: %d\n" price
    confirmed <- readConfirmation "Proceed with payment?"
    if confirmed
      then return (Just Confirmed)
      else return Nothing
```

``` haskell
newtype CouponsT m a = CouponsT (m a)
  deriving ( Monad
           , Functor
           , Applicative
           , MonadError e
           , MonadIO
           , Payments
           )
```

``` haskell
instance Checkout m => Checkout (CouponsT m) where
  type State (CouponsT m) = State m
  select = CouponsT . select
  getCoupon = CouponsT getCoupon
  getCards = CouponsT getCards
  selectCard = CouponsT . selectCard
  confirm card items price = CouponsT (confirm card items price)
```

``` haskell
runCouponsT :: MonadIO m => CouponsT m a -> m a
runCouponsT (CouponsT a) = a

instance MonadIO m => Coupons (CouponsT m) where
  lookupDiscount coupon =
    case coupon of
      Coupon "s3cr3t" -> return (Just (Discount 0.25))
      _ -> return Nothing

newtype PaymentT m a =
  PaymentT (m a)
  deriving ( Monad
           , Functor
           , Applicative
           , MonadError e
           , MonadIO
           , Coupons
           )
```

``` haskell
instance Checkout m => Checkout (PaymentT m) where
  type State (PaymentT m) = State m
  select = PaymentT . select
  getCoupon = PaymentT getCoupon
  getCards = PaymentT getCards
  selectCard = PaymentT . selectCard
  confirm card items price = PaymentT (confirm card items price)
```

``` haskell
runPaymentT :: MonadIO m => PaymentT m a -> m a
runPaymentT (PaymentT a) = a

instance MonadIO m => Payments (PaymentT m) where
  pay price card = return True
```

Foo bar

``` haskell
main :: IO ()
main = do
  let items = [ CartItem "Banana"
              , CartItem "Car"
              , CartItem "Insurance"
              ]
  e <- runExceptT (runPaymentT (runCouponsT (runCheckoutT (checkout items))))
  case e of
    Left err -> print err
    Right True -> print "OK!"
    Right False -> print "Failed."
```
