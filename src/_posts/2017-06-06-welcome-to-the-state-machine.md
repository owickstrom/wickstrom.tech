---
layout: post
title: Welcome to the State Machine
author: Oskar Wickström
categories: programming
tags: ["haskell", "functional", "dsl"]
excerpt: "Todo..."
published: false
---

``` haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AbstractStateMachines where

import Control.Monad.IO.Class
```

ATM State Machine
-----------------

As a warm-up example, we model an ATM (also known as cash point, cash dispenser, or bankomat,) using the abstract state machine style.

The valid states of the ATM are expressed as *uninhabited data types*, as they are used only as marker types.

``` haskell
data CardInserted
data PinEntered
```

The type class `MonadAtm` captures the valid state transitions for an ATM as methods. Each method returns a value of the associated `AtmT` type, parameterized by a state.

``` haskell
class MonadAtm m where
  type AtmT m :: * -> *

  insertCard :: m (AtmT m CardInserted)

  enterPin :: AtmT m CardInserted
           -> m (AtmT m PinEntered)
```

### A Console ATM

``` haskell
newtype ConsoleAtm a =
  ConsoleAtm { runConsoleAtm :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

data ConsoleAtmState s where
  CardInserted :: String -> ConsoleAtmState CardInserted
  PinEntered :: ConsoleAtmState PinEntered

instance MonadAtm ConsoleAtm where
  type AtmT ConsoleAtm = ConsoleAtmState
  insertCard = liftIO $ do
    putStrLn "Card number:"
    CardInserted <$> getLine
  enterPin _ = liftIO $ do
    return PinEntered
```
