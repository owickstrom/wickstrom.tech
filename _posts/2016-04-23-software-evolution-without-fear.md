---
layout: post
title: Software Evolution Without Fear
date: 2016-04-23 17:00
category: programming
tags: haskell tools languages type-systems
---

The software we create needs to evolve continuously. There are many reasons for
programs to evolve; changing business needs, bug fixes, shifting technical
requirements, performance, staff changes, quality improvements, the release of
the next big Javascript framework, etc. The list goes on and on.

In this post I offer my rather technical viewpoint on the subject &mdash; what
programming languages and tools can do to help us evolve software in a reliable
way, avoiding status quo or costly rewrites. I discuss dynamic and static
typing in different languages and how Haskell enables me to do large
changes to the Oden compiler without introducing programming errors.

## Scenario: Changing Business Needs

Let's say your job is to add a new shipping option in an E-commerce
application. The existing shipping code resides in the dreaded
`ECommerceControllerBaseFactory` class, weighing in at 7500+ lines of code.
Everyone in the team knows how complex this code is and still it continues to
thrive.

Eventually you find the four-level nested `if` statement doing lots of stuff
with shipping options, customer bonus points, country tax regulations, mixed
with all kinds of non-business concerns like logging, constructing and
destructuring of values and closing resources. There's small attempts at
abstraction in there, with private class methods small chunks of the work, but
it's still a big [complected][complected] mess of different capabilities and
technical necessities. If you've been in the "bread and butter" side of
systems development, I bet you've seen something like this before, both in back
and front end applications.

### Fear of Change

Why hasn't anyone refactored this mass of complexity? Having the giant elefant
in the room cannot possibly be a good thing, especially not in the part of the
code that models your core business. This is where you should have the best
code! I believe it usually starts out rather innocent. As the software evolves
over time, by people making many small changes, complexity steadily creeps in.
One additional `else if { ... }` cannot be that bad, right? Or one more nested
callback, surely we can live that?

From my experience these growing pieces of code doesn't get refactored out of
the fear of introducing bugs. Invalid business logic, null pointer exceptions,
various simple programming errors, concurrency bugs &mdash; a lot of things can
go wrong if you try to break this class into pieces and so we take the path of
least resistance.

It's just not about the lines of code and complexity. As a project evolve
developers usually come and go. People quit their jobs or start working in
other departments or roles, and new people join. And if the project grows it
it might get more developers working in parallel in the same code. Even if you
had the guts to make a big change to the massive class, you could spend weeks
with merge conflicts and thus have an even greater risk of errors. Also, as the
fragmentation of developers increase the individual developer might now feel
responsible for taking greater action.

When we continually opt for the change of least risk the situation does not
improve. We are often pushed by deadlines or sprints, limiting our time to add,
delete or modify the code in question. *"You only need to add the shipping
option, that's it! We don't have time for tests or refactoring right now"*,
you might be told.

One of way of dealing with these issues is to have a dedicated time slot for
quality improvements. While this is probably better than doing nothing at all,
working proactively with quality rather than retroactively is the way to go.

## Tools that Reduce Fear

Different programming languages and tools help us at varying degrees. You can
argue that a dynamically typed programming language is faster to prototype and
experiment with than statically typed alternatives. As much as I love hanging
out in a Scheme REPL for a couple of hours, trying stuff out and working my way
forward, that itching fear or changing existing code takes hold of me when a
project starts to grow.

There are tools for dynamically typed languages that aims to address these
issues. Racket has [Typed Racket][typedracket], a statically typed dialect of
Racket.  Clojure has [Typed Clojure][typedclojure], an optional type system for
Clojure that can validate your program at compile time. [Schema][schema] is
another option for Clojure that performs runtime data validation based on
contracts.

### The Haskell Experience

Programming in Haskell is another workflow for me. I personally don't rely much
on the GHCi REPL for experimentation. Instead the types guide me to the end
goal. That sounds cheesy, I know, but let me try to explain.

First off, *I think about it*. In the shower, before falling asleep, when
looking out the window, when riding the bike to work. Staring at code or
pressing keyboard buttons will not help me solve the problem at hand, I just
get stuck and become frustrated.

After thinking about it, I hopefully arrive at something that might work. I
start by modeling my thinking in terms of data types and function type
signatures. Functions are `undefined` until I'm ready write the implementation.
This is basically a top-down approach for materializing the design I have in my
head into code. It enables me to explore freely and also give me statically
typed code right from the start.

In an existing Haskell code base, given that everything is not typed as `IO
()`, making structural changes is not that scary. Having rewritten the Oden
compiler from Racket to Haskell I think comparing the two implementations is
reasonable. Lately I have been doing big changes to the core model of the
compiler, changing thousands of lines of code affecting the majority of source
files, and still I'm confident there will be no annoying programming errors,
null pointer exceptions or incorrectly interleaved effects. I actively avoided
making those kinds of changes to the Racket code base due to the risks of
introducing runtime errors.

I should point out that the Oden compiler is like a big pure function with no
external state, so it is easier to test than a system with integrations and
side effects. And yes, I can still introduce bugs that Haskell won't be able to
catch, but the majority of errors I would have to consider in a language like
Java or Scheme just isn't there.

## Let's Be Explicit

Lately I've been trying to become aware of how I think about functions and data
as I write programs. What I've found is that I generally think in terms of
algebraic data types, records with known sets of fields, monads and functors,
regardless if I'm programming in Haskell, Scheme or Javascript. *"Oh, that's
just like the List monad."*, I might think to myself. I consider this *my inner
type system* that I fall back to when there is no explicit type system in the
programming language. I suspect many of us think about code in a similar way.
If so, why not formalize that into an encoding that we can share between humans
and computers to improve quality? This is what I consider the essence of static
typing.

I am certain that we can have have mainstream programming languages with static
typing and still retain the expressiveness and exploratory power of dynamically
typed languages. I think we should strive for tools that help us fearlessly
evolve software. We can do a lot better than we are doing right now! Also,
static typing as we know it today might only be a stepping stone to something
even greater.


[complected]: https://en.wiktionary.org/wiki/complect
[typedracket]: https://docs.racket-lang.org/ts-guide/
[typedclojure]: http://typedclojure.org/
[schema]: https://github.com/plumatic/schema
