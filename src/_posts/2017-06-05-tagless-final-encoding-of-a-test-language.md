---
layout: post
title: Tagless Final Encoding of a Test Language
author: Oskar Wickström
categories: programming
tags: ["haskell", "functional", "dsl"]
---

I have experimented with a test language encoded in tagless final style, instead of algebraic data types, to support the typed combinators *beforeEach* and *beforeAll*. Although the intended use is for [PureScript Spec](http://purescript-spec.wickstrom.tech/), I want to share the Haskell prototype I ended up with, and explain how I got there.

The Algebraic Data Type Approach
--------------------------------

The PureScript Spec project, inspired by Haskell's [hspec](http://hspec.github.io/), provides an EDSL and framework for writing, organizing, and running PureScript tests. Combinators use a State monad collecting tests into an algebraic data structure, representing the test language tree structure.

``` haskell
describe "My Module" $ do
  describe "Feature #1" $ do
    it "does addition" (1 + 1 `shouldEqual` 2)
    it "does subtraction" (1 - 1 `shouldEqual` 0)
  describe "Feature #2"
    it "does multiplication" (2 * 2 `shouldEqual` 4)
```

The `Group` data type holds *describe* groups and *it* tests, here shown in a simplified form, and translated to Haskell. The test *body* has the paramaterized type `t`, making the `Group` structure suitable for representing not only tests to be run, but also for test results.

``` haskell
data Group t
  = Describe String [Group t]
  | It String t
```

A test suite to be run can have type `[Group (IO ())]`, and a test result can have type `[Group TestResult]`.

In a GitHub pull request for PureScript Spec, we discussed how to implement setup and tear-down functions around tests, and how to make them type safe. I started poking around in the codebase, but soon realized that the required change was larger than I first imagined, and so I began on a clean slate prototype. The reason I used Haskell was to focus more on modeling different alternatives, and less time on hand-written instances for newtypes.

I wanted a setup function to provide a return value, and all tests run with the setup to receive the return value as a parameter. Thus, *n* setup functions would require test functions of *n* arguments. A test with an incorrect number of arguments would give a type error at compile-time.

My first attempt was to extend the current design by adding a new constructor `BeforeEach` to the `Group` data type. Using the already parameterized test body, tests inside a `BeforeEach` would be functions from the return value of type `b`, to some test body of type `t`. For each nesting of `BeforeEach`, test body types would get an additional argument.

``` haskell
data Group b t
  = Describe String [Group b t]
  | It String t
  | BeforeEach (IO b) (Group b (b -> t))
```

While this structure can hold multiple nested `BeforeEach` values, and enforce the correct number of arguments to `It` body functions, the type `b` cannot vary throughout the structure. Requiring all setup functions in a test suite to return values of the same type was not acceptable. I suspect that there might be a way to solve this in Haskell using GADTs and existential types, but I'm not sure how it would translate to PureScript.

Following the idea of parameterizing `Group` further, I'd probably end up close to a specialized version of the Free monad. [Why free monads matter](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html) explains a similar path, arriving at the Free monad, most eloquently. I decided, however, to try out a [tagless final style](http://okmij.org/ftp/tagless-final/index.html) encoding for the test language in my Haskell prototype.

Exploring Tagless Final Encoding
--------------------------------

Having kept an eye out for practical examples of tagless final style, I was keen on trying it out for the test language. The discussion started on a local meetup in Malmö, where I presented the problem, together with my suspicion that a combination of tagless final style encoding and Applicative would solve it elegantly. The following design is the result of my own explorations after the meetup, and will hopefully be of use for the real implementation in PureScript Spec in the future.

As this article is a runnable program in Literate Haskell form, we begin by declaring a bunch of language extensions, the module, and our imports.

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

Encoding the test language in tagless final style means that all operations are overloaded functions in a type class. `MonadSpec` takes two type arguments; `m` and `f`, constrained to `Monad` and `Applicative`, respectively. The class includes the operations `it`, `describe`, `beforeEach`, and `beforeAll`.

``` haskell
class (Monad m, Applicative f) => MonadSpec m f where
```

The operations of the type class constitute the whole language. Beginning with the leaf operation `it`, we see that it takes a string description, some test of type `a`, and returns a `Spec` parameterized by `m` and `(f a)`.

``` haskell
  it :: String -> a -> Spec m (f a)
```

Having the test, a value of type `a`, wrapped up inside the `(Applicative f)` is essential for our operations to compose.

The `describe` operation takes a string describing a group of tests, and another Spec, with any type of tests, as long as they are inside the `(Applicative f)`.

``` haskell
  describe :: String -> Spec m (f a) -> Spec m (f a)
```

The setup combinators `beforeEach` and `beforeAll` have identical type signatures. They take a setup value of type `(f a)`, and a Spec with tests of type `(f (a -> b))`, returning a Spec with tests of type `(f b)`. The type shows that the applicative test functions are applied by the setup combinators.

``` haskell
  beforeEach :: f a -> Spec m (f (a -> b)) -> Spec m (f b)
  beforeAll  :: f a -> Spec m (f (a -> b)) -> Spec m (f b)
```

What is a `Spec`? A Writer monad transformer, collecting `Group` values. Using an explicit `WriterT` is needed for the interpreter, explained shortly, to capture nested tests, apply test functions to setup combinators' return values, and change the type of the test structure during interpretation.

``` haskell
type Spec m a = WriterT [Group a] m ()
```

The `Collector` interpreter collects a Spec into an data structure, much like the original approach, but *with all test functions fully applied* inside the applicative.

``` haskell
newtype Collector m a = Collector { unCollector :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           )
```

The `Group` data structure holds the applied test functions, and thus has no constructors for `BeforeEach` and `BeforeAll`.

``` haskell
data Group a
  = Describe String [Group a]
  | It String a
  deriving (Functor)
```

In effect, the `Collector` interpreter loses information when collecting the test suite as a `Group` data structure. Other interpreters, e.g. a test suite pretty-printer, would provide its own instance of the `beforeEach` and `beforeAll` operations, and retain the setup information for its particular purpose.

The `MonadSpec` instance for the `Collector` interpreter defines the applicative type parameter as `IO`. This means that all setup combinator values must have type `(IO a)`, where the nested test functions have type `(IO (a -> b))`.

``` haskell
instance (Monad m, MonadIO m) => MonadSpec (Collector m) IO where
```

The `it` instance wraps the test body inside the applicative, and reports the `Group` in the `WriterT` monad.

``` haskell
  it name body =
    tell [It name (return body)]
```

To wrap test groups in a `Describe` value, the instance of `describe` runs the nested `WriterT` to collect all groups.

``` haskell
  describe name spec = do
    groups <- lift $ execWriterT spec
    tell [Describe name groups]
```

The interesting part is the `beforeEach` instance, where the inner test function is applied using `(<*>)`. As the `Group` data type has an instance of `Functor`, the application can be mapped recursively over the structure using `fmap`.

``` haskell
  beforeEach setup spec = do
    groups <- lift $ execWriterT spec
    tell $ fmap (<*> setup) <$> groups
```

This is where `WriterT` must be explicit in the `MonadSpec` operations. In a previous attempt, I had a `MonadWriter` constraint on the interpreter, and no mention of `WriterT` in `MonadSpec`. That approach prohibited the monoidal type of `MonadWriter` to change during interpretation, a change required to apply test functions when interpreting setup combinators. Instead, by making `WriterT` explicit in the `MonadSpec` operations, the `Collector` instance can collect groups using `lift` and `execWriterT`, and freely change the test function types.

As a slight variation on `beforeEach`, the `beforeAll` combinator must only run the setup action once, applying all test functions with the same return value. Using the [io-memoize](https://hackage.haskell.org/package/io-memoize) package, and the existing `beforeEach` combinator, we can do this neat trick:

``` haskell
  beforeAll setup spec = do
    s <- liftIO $ once setup
    beforeEach s spec
```

Collecting all tests, fully applied with setup return values, is a matter of running the `WriterT` and `Collector` instances, and joining the applicative values with the test body values, here constrained to be the same monadic type `m`. Note that Applicative is a super class of Monad since GHC 7.10.

``` haskell
collect
  :: Monad m
  => Spec (Collector m) (m (m a))
  -> m [Group (m a)]
collect spec = do
  groups <- unCollector $ execWriterT spec
  return (map (fmap join) groups)
```

Using `collect`, the `run` function traverses the group of tests, printing and running all tests, in the `(MonadIO m)` instance.

``` haskell
run :: MonadIO m => Spec (Collector m) (m (m ())) -> m ()
run spec = do
  groups <- collect spec
  mapM_ (go []) groups
  where
    go ctx (Describe name groups) =
      mapM_ (go (ctx ++ [name])) groups
    go ctx (It name body) = do
      let heading = intercalate " > " ctx ++ " > it " ++ name
      liftIO $ putStrLn heading
      body
```

We can now nest setup combinators, describe groupings, and tests, with different types, while retaining type safety. The test suite type signature is somewhat verbose, but that could be hidden with a type alias. The only drawback, as I see it, is that the outer setup combinators provide the right-most test function parameters, which feels a bit backwards from a user point of view.

``` haskell
mySpec :: MonadSpec m IO => Spec m (IO (IO ()))
mySpec =
  beforeAll (putStrLn "once, before all!" >> return "foo") $ do

    describe "module 1" $
      beforeEach (putStrLn "before each 1!" >> return 20) $
        describe "feature A" $ do
          it "works!" (\x y -> assert (x == 20))
          it "works again!" (\x y -> assert (y == "foo"))

    describe "module 2" $
      beforeEach (putStrLn "before each 2!" >> return 30) $
        describe "feature B" $ do
          it "works!" (\x y -> assert (x == 30))
          it "works again!" (\x y -> assert (y == "foo"))
  where
    assert True = return ()
    assert False = fail "Test failure!"
```

Using `IO` and `fail` to report test failures would be less then ideal a real test suite, but it serves our purposes in this experiment. OK, let's run the test suite, already!

``` haskell
main :: IO ()
main = run mySpec
```

Looking at the output, we see that "once, before all!" is printed only once, and that "before each ..." is printed for every test.

    *Test.Spec> :main
    module 1 > feature A > it works!
    before each 1!
    once, before all!
    module 1 > feature A > it works again!
    before each 1!
    module 2 > feature B > it works!
    before each 2!
    module 2 > feature B > it works again!
    before each 2!

Summary
-------

Using tagless final style for embedded languages is fun and powerful. I hope this demonstration can serve as a motivating example for readers interested in EDSLs and tagless final style, in addition to it perhaps influencing or finding its way into the PureScript Spec project. It would also be interesting to explore a Free monad approach, and to compare the two.
