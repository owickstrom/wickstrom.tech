---
title: Talks
layout: page
---

## The Power of Functional Programming and Static Type Systems in Server-Side Web Applications

### Events

* [flatMap(Oslo)](http://2017.flatmap.no/talks/wickstrom/), 2-3 May 2017, Oslo, Norway
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
