---
title: Talks
layout: page
---

## Writing a Screencast Video Editor in Haskell


### Events

* [Lambda World](http://cadiz.lambda.world), October 25-26, 2018, Cádiz
  - [Slides](https://owickstrom.github.io/writing-a-screencast-video-editor-in-haskell/)
  - [Video Recording](https://www.youtube.com/watch?v=psasUATsjQw)

### Abstract

In dire need of better tools when producing screencasts for Haskell at Work, I started building Komposition, the video editor for screencasters. This desktop application automatically detects scenes in screen capture video, detects sentences in audio parts, and features and a high-productivity editing workflow keyboard-driven navigation.

I’ll talk about why I started this project, and share my experiences building Komposition using Haskell. I’ll describe its architecture, what packages and techniques I’ve used, and outline the plans for its future.

## Declarative GTK+ Programming in Haskell

### Events

* [LambdAle](https://lambdale.org/), September 1, 2018, London
    - [Slides](https://owickstrom.github.io/declarative-gtk-programming-in-haskell/)
    - [Video Recording](https://www.youtube.com/watch?v=mdQRffXBn0s)

### Abstract

Functional programming has made a substantial impact on user interface programming for the web, with virtual DOM technologies and unidirectional data flow. Can the same be done in Haskell without compiling to Javascript and Electron? In this talk you will learn about declarative programming of GTK interfaces with Haskell.

https://lambdale.org/

## Fast and Fearless Evolution of Server-Side Web Applications

### Events

* [f(by)](https://fby.by/), December 17, 2017
    - [Slides](https://github.com/owickstrom/fast-and-fearless-evolution-of-server-side-webapps)
    - [Video Recording](https://www.youtube.com/watch?list=PLpVeA1tdgfCCUuAtFl0N5wzatXx0gWLKM&v=JKRkR_ZQSBc)

### Abstract

When evolving web applications, in most programming languages and frameworks,
we risk introducing programming errors. Undefined values, parsing
failures, broken links, invalid markup, and good old null pointers, are
all things that can break our applications. Manually writing and
maintaining tests to catch programming errors is a time consuming effort,
and we would rather spend that time testing our application logic. This
talk takes you on a whirlwind tour of mature technologies that offer
static guarantees for modern web applications.

## Finite-state machines? Your compiler wants in!

### Events

* [Foo Cafe](http://www.foocafe.org/malmoe/events/1596-finite-state-machines-your-compiler-wants-in), November 1, 2017, Malmö
* [CodeMesh](http://www.codemesh.io/codemesh2017/oskar-wickstrom), November 7-9, 2017, London
    - [Slides](http://s3.amazonaws.com/erlang-conferences-production/media/files/000/000/756/original/Oskar_Wickstrom_-_Finite-state_machines__Your_compiler_wants_in!.pdf?1510133482)
    - [Video Recording](https://www.youtube.com/watch?v=GWqsmzRpao8)
* [BOB konferenz](https://bobkonf.de/2018/wickstroem.html), February 23, 2018, Berlin
    - [Video Recording](https://www.youtube.com/watch?v=5KvsuwspXZI)

### Abstract

When modeling problem domains, we collect different possible states,
legal transitions between states, and relevant data for each
state. Finite-state machines emerge. To verify that programs are
constructed correctly, and to have a living machine-verified
documentation, we should let the compiler in on our trade secrets.

In this talk we will look at motivations and examples of encoding
finite-state machines, using expressive type systems in functional
languages.

## The Power of Functional Programming and Static Type Systems in Server-Side Web Applications

### Events

* [flatMap(Oslo)](http://2017.flatmap.no/talks/wickstrom/), 2-3 May 2017, Oslo, Norway
    - [Slides](https://wickstrom.tech/talks/flatmap-oslo-2017-05-02.pdf)
    - [Video Recording](https://vimeo.com/216464016)
* [GOTO Nights CPH](https://www.meetup.com/GOTO-Nights-CPH/), February 22, 2017, Copenhagen, Denmark
    - [Slides](https://wickstrom.tech/talks/kats-conf-2017-02-18.pdf)
* [Kats Conf 2](http://www.katsconf.com/), February 18, 2017, Dublin, Ireland
    - [Slides](https://wickstrom.tech/talks/kats-conf-2017-02-18.pdf)

### Abstract

Single-page web applications have been spreading like wildfire, with an endless
amount of frameworks and libraries, and functional programming together with
static types fueling the fire with great ideas like pure functions, monads, and
strong type checking. But what happened to Progressive Enhancement? Some parts
of our applications might require Javascript to function, but the majority
could be built with ordinary links and forms.  In this talk we will explore how
we can build web applications using established web technology, and the power
of functional programming and PureScript on the server-side, with strong
correctness guarantees.

## Oden - A Functional Programming Language for the Go Ecosystem

### Events

* [PolyConf 2016, Poznan, Poland](https://16.polyconf.com/)
    - [Slides](https://speakerdeck.com/owickstrom/oden-a-functional-programming-language-for-the-go-ecosystem-polyconf-2016)
    - [Video Recording](https://www.youtube.com/watch?v=qRm_58RA9ns)
* [Curry On 2016, Rome, Italy](http://curry-on.org/2016/)
    - [Slides](https://speakerdeck.com/owickstrom/oden-a-functional-programming-language-for-the-go-ecosystem-curry-on-2016)
    - [Video Recording](https://www.youtube.com/watch?v=t_bR2UBEmp0&t=1s)

### Abstract

This talk will introduce Oden, an experimental, statically typed, functional
programming language being built for the Go ecosystem. We will look at how Oden
aims to leverage the great features of Go — static linking, cross-compilation,
goroutines, channels and the great set of libraries and tools — and enable
higher-level abstractions, generics and a safer yet more flexible type system.

## Designing and Building the Oden Programming Language

### Events

* [Foo Cafe, April 13, 2016](http://www.foocafe.org/malmoe/events/1115-designing-and-building-the-oden-programming-language)
    - [Slides](https://speakerdeck.com/owickstrom/designing-and-building-the-oden-programming-language)
    - [Video Recording](https://www.youtube.com/watch?v=XEaWpHqgpsI)

### Abstract

This first part of this talk will introduce Oden, an experimental, statically
typed, functional programming language being built for the Go ecosystem. We
will look at how Oden aims to leverage the great features of Go — static
linking, cross-compilation, goroutines, channels and the great set of libraries
and tools — and enable higher-level abstractions, generics and a safer yet more
flexible type system.

The second part will delve more deeply into the implementation of the Oden
compiler. Why was it first written in Racket and then rewritten in Haskell?
What pros and cons are there in writing compilers in Haskell? We will look at
how the type system can help us build safe and robust intermediate
representations and transformations between them.
