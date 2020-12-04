---
layout: post
title: "Motor: Finite-State Machines in Haskell"
author: Oskar Wickström
categories: programming
tags: ["haskell", "library", "fsm"]
excerpt: "While writing my talk for CodeMesh, I have worked on porting the Idris ST library to Haskell. I call it Motor."
---

While writing my talk ["Finite-state machines? Your compiler wants
in!"](https://wickstrom.tech/talks.html), I have worked on porting the Idris
[ST](http:/docs.idris-lang.org/en/latest/st/state.html) library to Haskell.
I call it *Motor*.

Motor is an *experimental* Haskell library for building finite-state
machines with type-safe transitions and effects. I have just
published it on [Hackage][hackage], written a bunch of documentation with
Haddock, and put the source code on [GitHub][repo].

This blog post is very similar to the Hackage documentation, and aims to pique
your interest. The library and documentation will probably evolve and outdate
this description, though.

## State Machines using Row Types

The central finite-state machine abstraction in Motor is the
`MonadFSM` type class. `MonadFSM` is an *indexed monad* type class, meaning
that it has not one, but *three* type parameters:

1. A `Row` of input resource states
1. A `Row` of output resource states
1. A return type (just as in `Monad`)

The `MonadFSM` parameter kinds might look a bit
scary, but they state the same:

``` haskell
class IxMonad m =>
  MonadFSM (m :: (Row *) -> (Row *) -> * -> *) where
  ...
```

The rows describe how the FSM computation will affect the state of its
resources when evaluated. A row is essentially a type-level map, from
resource names to state types, and the FSM computation\'s rows describe
the resource states *before* and *after* the computation.

An FSM computation `newConn` that adds a resource named `"connection"`
with state `Idle` could have the following type:

``` haskell
newConn :: MonadFSM m =>
  m r ("connection" ::= Idle :| r) ()
```

A computation `spawnTwoPlayers` that adds two resources could have this
type:

``` haskell
spawnTwoPlayers :: MonadFSM m =>
  m r ("hero2" ::= Standing :| "hero1" ::= Standing :| r) ()
```

Motor uses the extensible records in `Data.OpenRecords`, provided by the
[CTRex](https://wiki.haskell.org/CTRex "https://wiki.haskell.org/CTRex")
library, for row kinds. Have a look at it\'s documentation to learn more
about the type-level operators available for rows.

## Building on Indexed Monads

As mentioned above, `MonadFSM` is an indexed monad. It uses the definition from
`Control.Monad.Indexed`, in the
[indexed](https://hackage.haskell.org/package/indexed-0.1.3) package. This
means that you can use `ibind` and friends to compose FSM computations.

``` haskell
-- 'c1' and 'c2' are FSM computations
c1 >>>= \_ -> c2
```

You can combine this with the `RebindableSyntax` language extension to
get do-syntax for FSM programs:

``` haskell
test :: MonadFSM m => m Empty Empty ()
test = do
  c1
  c2
  r <- c3
  c4 r
  where
    (>>) a = (>>>=) a . const
    (>>=) = (>>>=)
```

See [24 Days of GHC Extensions: Rebindable
Syntax](https://ocharles.org.uk/blog/guest-posts/2014-12-06-rebindable-syntax.html "https://ocharles.org.uk/blog/guest-posts/2014-12-06-rebindable-syntax.html")
for some more information on how to use `RebindableSyntax`.

## State Actions

To make it easier to read and write FSM computation types, there is some
syntax sugar available.

*State actions* allow you to describe state changes of named resources
with a *single* list, as opposed two writing two rows. They also take
care of matching the CTRex row combinators with the expectations of
Motor, which can be tricky to do by hand.

There are three state actions:

-   `Add` adds a new resource.
-   `To` transitions the state of a resource.
-   `Delete` deletes an existing resource.

A mapping between a resource name is written using the `:->` type operator,
with a `Symbol` on the left, and a state action type on the right. Here are
some examples:

``` haskell
"container" :-> Add Empty

"list" :-> To Empty NonEmpty

"game" :-> Delete GameEnded
```

So, the list of mappings from resource names to state actions describe
what happens to each resource. Together with an initial row of resources
`r`, and a return value `a`,
we can declare the type of an FSM computation using the
`Actions` type:

``` haskell
MonadFSM m => Actions m '[ n1 :-> a1, n2 :-> a2, ... ] r a
```

A computation that adds two resources could have the following type:

``` haskell
addingTwoThings ::
  MonadFSM m =>
  Actions m '[ "container" :-> Add Empty
              , "game" :-> Add Started
              ] r ()
```

## Infix Operators

As an alternative to the `Add`, `To`, and `Delete` types, Motor offers
infix operator aliases. These start with `!` to indicate that they can
be effectful.

The `!-->` operator is an infix alias for
`To`:

``` haskell
useStateMachines ::
  MonadFSM m =>
  Actions m '[ "program" :-> NotCool !--> Cool ] r ()
```

The `!+` and `!-` are infix
aliases for mappings from resource names to `Add`
and `Delete` state actions, respectively:

``` haskell
startNewGame ::
	MonadFSM m =>
	Actions m '[ "game" !+ Started ] r ()
```

``` haskell
endGameWhenWon ::
	MonadFSM m =>
	Actions m '[ "game" !- Won ] r ()
```

## Row Polymorphism

Because of how CTRex works, FSM computations that have a free variable
as their input row of resources, i.e. that are polymorphic in the sense
of other resource states, must list *all their actions in reverse
order*.

``` haskell
doFourThings ::
     Game m
  => Actions m '[ "hero2" !- Standing
                , "hero1" !- Standing
                , "hero2" !+ Standing
                , "hero1" !+ Standing
                ] r ()
doFourThings = do
  spawn hero1
  spawn hero2
  perish hero1
  perish hero

  where
    (>>) a = (>>>=) a . const
    (>>=) = (>>>=)
```

This is obviously quite clumsy. If anyone has ideas on how to fix or work
around it, *please get in touch*. Had the `r` been replaced by `Empty` in the
type signature above, it could have had type `NoActions m Empty ()` instead.

## Running the State Machine

The `runFSM` function in `Motor.FSM` runs an FSM computation in some base
monad:

``` haskell
runFSM :: Monad m => FSM m Empty Empty a -> m a
```

`FSM` has instances for `IxMonadTrans` and a bunch of other type classes. More
might be added as they are needed.

## Examples

There is only [one small Door
example](https://github.com/owickstrom/motor/tree/master/examples/Door.hs) in
the repository, along with some test programs. I haven't had much time to write
examples, but hopefully I will soon. The door example does feature most of the
relevant concepts, though.

[repo]: https://github.com/owickstrom/motor
[hackage]: http://hackage.haskell.org/package/motor
