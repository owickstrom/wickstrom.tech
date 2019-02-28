{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Prelude        hiding (reverse)

import           Control.Monad  (unless)
import           Data.List      (sort)

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

-- start snippet reverse
prop_reverse = property $ do
  xs <- forAll $
    Gen.list
    (Range.linear 0 10)
    (Gen.int Range.linearBounded)
  reverse (reverse xs) === xs
-- end snippet reverse

-- start snippet sort
prop_sort = property $ do
  xs <- forAll $
    Gen.list
    (Range.linear 0 10)
    (Gen.int Range.linearBounded)
  mySuperSort xs === industryStandardSort xs
-- end snippet sort

-- FAKE IMPLEMENTATIONS

reverse :: [a] -> [a]
reverse = id

mySuperSort :: Ord a => [a] -> [a]
mySuperSort = sort

industryStandardSort :: Ord a => [a] -> [a]
industryStandardSort = sort

-- RUNNER

main :: IO ()
main = do
  res <- checkParallel $$discover
  unless res $
    fail "Tests failed."
