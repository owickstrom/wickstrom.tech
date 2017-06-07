> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module AbstractStateMachines where
>
> import Control.Monad
> import Control.Monad.IO.Class

ATM State Machine
=================

As a warm-up example, we model an ATM (also known as cash point, cash
dispenser, or bankomat,) using the abstract state machine style.

The valid states of the ATM are expressed as _uninhabited data types_,
as they are used only as marker types.

> data MachineIdle
> data CardInserted
> data PinVerified

The type class `MonadAtm` captures the valid state transitions for an
ATM as methods. Each method returns a value of the associated `AtmT`
type, parameterized by a state.

> type Amount = Int
>
> newtype InvalidPin = InvalidPin String
>
> class MonadAtm m where
>   type AtmState m :: * -> *
>
>   reset
>     :: m (AtmState m MachineIdle)
>
>   insertCard
>     :: AtmState m MachineIdle
>     -> m (AtmState m CardInserted)
>
>   enterPin
>     :: AtmState m CardInserted
>     -> m (Either InvalidPin (AtmState m PinVerified))
>
>   withdraw
>     :: Amount
>     -> AtmState m PinVerified
>     -> m (AtmState m MachineIdle)

A Console ATM
-------------

> newtype ConsoleAtm a =
>   ConsoleAtm { unConsoleAtm :: IO a }
>   deriving (Functor, Applicative, Monad, MonadIO)
>
> data ConsoleAtmState s where
>   MachineIdle :: ConsoleAtmState MachineIdle
>   CardInserted :: String -> ConsoleAtmState CardInserted
>   PinVerified :: ConsoleAtmState PinVerified
>
> instance MonadAtm ConsoleAtm where
>   type AtmState ConsoleAtm = ConsoleAtmState
>   reset = return MachineIdle
>   insertCard _ = liftIO $ do
>     putStrLn "Card number:"
>     CardInserted <$> getLine
>   enterPin _ = liftIO $ do
>     putStrLn "PIN code:"
>     pin <- getLine
>     if pin == "secret"
>       then return (Right PinVerified)
>       else return (Left (InvalidPin pin))
>   withdraw amount _ =
>     return MachineIdle
>
> runConsoleAtm :: ConsoleAtm (ConsoleAtmState MachineIdle) -> IO ()
> runConsoleAtm = void . unConsoleAtm

> giveMeSomeCash = runConsoleAtm $ do
>   r <- reset >>= insertCard >>= enterPin
>   case r of
>     Left InvalidPin{} -> do
>       liftIO $ putStrLn "Invalid PIN!"
>       reset
>     Right pin -> do
>       idle <- withdraw 100 pin
>       liftIO $ putStrLn "Here you go, a 100 euros!"
>       return idle
