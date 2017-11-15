module Checkout where

import           Data.List.NonEmpty
import           Data.Text          (Text)
import qualified Data.Text          as T

type Price = Double

data CartItem =
  CartItem
  { itemId    :: Text
  , itemPrice :: Price
  }
  deriving (Show, Eq)

newtype Card =
  Card Text
  deriving (Show, Eq)

calculatePrice :: NonEmpty CartItem -> Price
calculatePrice = foldl go 0
  where
    go sum item = itemPrice item + sum
