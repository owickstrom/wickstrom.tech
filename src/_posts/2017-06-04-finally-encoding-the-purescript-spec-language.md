---
layout: post
title: Finally Encoding the PureScript Spec Language
author: Oskar Wickström
categories: programming
tags: ["haskell", "functional", "dsl"]
excerpt: "To support combinators as beforeEach and beforeAll, the PureScript Spec language was modified to use final encoding. This article shows the design, but in Haskell."
---

Adipisicing earum atque maiores expedita labore. Consequuntur dolor itaque aut quaerat dignissimos provident saepe deserunt dicta ipsum magnam! Nostrum voluptatem ipsum tempore aspernatur obcaecati labore! Blanditiis quae dolor eveniet perferendis.

Data Structure Representation
-----------------------------

Sit aliquid rerum ratione quaerat repellendus illum reiciendis quae quis obcaecati. Numquam alias quae culpa aspernatur dicta. Consequatur facere dolore doloremque similique quidem esse repellat quaerat sunt nobis sunt eveniet?

Finally Tagless Encoding
------------------------

As this article is compiled from *Literate Haskell source code*, we begin by declaring a bunch of language extensions, the module, and our imports.

``` haskell
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Test.Spec where

import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.List
import           System.IO.Memoize
```

The core language is expressed in *final encoding*, meaning that all operations are overloaded functions in a type class. `MonadSpec` takes two type arguments; a Monad `m`, and an Applicative `f`, and includes the operations `it`, `describe`, `beforeEach`, and `beforeAll`.

``` haskell
class (Monad m, Applicative f) => MonadSpec m f where
```

The operations of the type class constitute the whole language. Beginning with the leaf operation `it`, we see that it takes a string description, some test of type `a`, and returns a `Spec` parameterized by `m` and `f a`.

``` haskell
  it :: String -> a -> Spec m (f a)
```

Having the test, a value of type `a`, wrapped up inside the Applicative `f` is essential for our operations to compose. For now, though, it is enough to think of `it` as way of constructing a `Spec` from a single test.

The `describe` operation

``` haskell
  describe :: String -> Spec m (f a) -> Spec m (f a)
  beforeEach ::  f a -> Spec m (f (a -> b)) -> Spec m (f b)
  beforeAll ::  f a -> Spec m (f (a -> b)) -> Spec m (f b)
```

``` haskell
type Spec m a = WriterT [Group a] m ()
```

``` haskell
newtype Runner a = Runner { unRunner :: IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           )

data Group a
  = Describe String [Group a]
  | It String a
  deriving (Functor)

instance MonadSpec Runner IO where
  it name body =
    tell [It name (return body)]

  describe name spec = do
    groups <- lift $ execWriterT spec
    tell [Describe name groups]

  beforeEach setup spec = do
    groups <- lift $ execWriterT spec
    tell $ fmap (<*> setup) <$> groups

  beforeAll setup spec = do
    s <- liftIO $ once setup
    beforeEach s spec

run :: Spec Runner (IO (IO ())) -> IO ()
run spec = do
  groups <- unRunner $ execWriterT spec
  let flattenedGroups = map (fmap join) groups
  mapM_ (go []) flattenedGroups
  where
    go ctx (Describe name groups) =
      mapM_ (go (ctx ++ [name])) groups
    go ctx (It name body) = do
      putStrLn $ intercalate " > " ctx ++ " > it " ++ name
      body
```

Using the New Language
----------------------

``` haskell
mySpec ::  MonadSpec m IO => Spec m (IO (IO ()))
mySpec =
  beforeAll (putStrLn "once, before all!" >> return 10) $ do
    describe "module 1" $
      beforeEach (putStrLn "before each 1!" >> return 20) $
        describe "feature A" $ do
            it "works!" (\x y -> print (x == 20))
            it "works again!" (\x y -> print (y == 10))
    describe "module 2" $
      beforeEach (putStrLn "before each 2!" >> return 30) $
        describe "feature B" $ do
            it "works!" (\x y -> print (x == 30))
            it "works again!" (\x y -> print (y == 10))

main :: IO ()
main = run mySpec
```
