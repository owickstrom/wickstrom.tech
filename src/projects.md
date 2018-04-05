---
title: Projects
layout: page
---

These are some of the projects I am, or have been, working on. All open-source
programming projects listed below, and many more, are available at [my GitHub
profile page](https://github.com/owickstrom).

## Haskell at Work Screencasts

[Haskell at Work](https://haskell-at-work.com/) is a screencast focused on
practical Haskell programming. Viewers should have a basic understanding of
Haskell, and be eager to learn new ways of working with Haskell. Check out the
site and subscribe to the YouTube channel if you want to learn more about
Haskell in the wild!

## Hyper

The goal of [Hyper](http://hyper.wickstrom.tech) is to make use of row
polymorphism, and other tasty type system features in [PureScript], to enforce
correctly stacked middleware in HTTP server applications. All effects of
middleware should be reflected in the types to ensure that otherwise common
mistakes cannot be made. This is one of my most recent projects. Please have a
look at the [the documentation](http://hyper.wickstrom.tech) for more
information.

## PureScript Spec

[PureScript Spec](http://owickstrom.github.io/purescript-spec/) is a testing
framework for PureScript, which I have maintained since April, 2015. It is
inspired by the Haskell testing frame [hspec](http://hspec.github.io/), and
supports synchronous and asynchronous tests using a simple DSL,
interoperability with other testing tools, and test output in various
formats.

## The Oden Programming Language

As described at [oden-lang.github.io], *Oden is an experimental,
statically typed, functional programming language, built for the Go
ecosystem.* I worked on the language for the majority of 2016, rounding off
in October. Why I stopped working on Oden is explained in greater detail in
the blog post [Taking a Step Back from
Oden](/programming/2016/10/10/taking-a-step-back-from-oden.html).  The
compiler is written in Haskell, and I am very satisified with the
readability of the source code, and the correctness of the compiler. I hope
that the source code can be of use, or serve as inspiration, to others.

## DataFlow

During my work at Sony Mobile, I created [DataFlow], a tool that renders graphs
using a declarative markup. It is built around the [DFD] format, but also
supports sequence diagrams and structured data output. We used it to document
our service integrations and security requirements between separate systems,
and integrated it in to our bigger documentation workflow. The software is
written in Haskell and has seen very few bugs.

[PureScript]: http://www.purescript.org/
[oden-lang.github.io]: https://oden-lang.github.io
[DataFlow]: https://github.com/sonyxperiadev/dataflow
[DFD]: https://en.wikipedia.org/wiki/Data_flow_diagram
