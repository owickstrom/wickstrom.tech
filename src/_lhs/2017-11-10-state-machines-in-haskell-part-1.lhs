Stateful programs often become complex beasts when they grow. Program
state incohesively spread across a bunch of variables, spuriously
guarded by even more variables, is what I refer to as *implicit
state*. When working with such code, we have to reconstruct a model
mentally, identifying possible states and transitions between them, to
modify it with any certainty. That process is tedious and error-prone,
and I insist we should have our tools do the heavy lifting instead.

By telling the compiler and type system about possible states and
state transitions in our program, it can verify that we follow our own
business rules, both when we write new code, and when we modify
existing code. It is not merely a process of asking the compiler "did
I do okay?" Our workflow can be a conversation with the compiler, a
process known as *type-driven development.*

After having given my talk at Code Mesh on this topic, and having
spent a lot of time researching and preparing examples, I want to
share the material in the form of blog posts. I will split it into
multiple posts, covering increasingly advanced techniques that give
greater type safety. That is not to say that the latter techniques are
inherently better, nor that they are the ones that you should
use. This series is meant as a small ala carte of state machine
encodings and type safety, where you can choose from the menu based on
your taste and budget. I will, however, present the techniques in a
linear form. Also, note that these posts do not claim to exhaust all
options for state-machine encodings.

There are many trade-offs, including type safety and strictness,
implementation complexity, and the effect to language choices in your
team. Taking one step towards *explicit state*, in an area where it
leverages your project doing so, can be the best choice. You don't
have to go nuts with type systems to use explicit states in your
program! Furthermore, most mainstream languages today let you encode
states as data types in some way.

![Our journey begins in the Valley of Programmer Death, deep in the
 lands of Implicit State. Follow along for as long as you like, and
 settle down in a place of your choice.](/assets/fsm-map.png)

This is the introductory post, in which I'll show the first step on
our way from implicit state and despair to writing stateful and
effectful programs with great confidence. We will use Haskell and
*algebraic data types* (ADTs) to encode possible states as data types.

Example: Shopping Cart Checkout
===============================

The running example we will use in these posts is a *shopping cart
checkout*, modelled as a finite-state machine. This stems from a
real-world project I worked on, where the lack of explicit states
in code became a real problem as requirements evolved. It's the
use-case that triggered my search for more robust methods.

![The running example, a shopping cart checkout.](/generated/uml/checkout.svg)

States as Data Types
====================

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module AbstractStateMachines where
>
> import Control.Monad
> import Control.Monad.IO.Class

Hello.
