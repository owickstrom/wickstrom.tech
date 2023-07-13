---
title: "Property-Based Testing in a Screencast Editor: Introduction"
author: Oskar Wickström
date: March 02, 2019
---

This is the first in a series of posts about using property-based
testing (PBT) within _Komposition_, a screencast editor that I've been
working on during the last year. It introduces PBT and highlights some
challenges in testing properties of an application like Komposition.

Future posts will focus on individual case studies, covering
increasingly complex components and how they are tested. I'll reflect
on what I've learned in each case, what bugs the tests have found, and
what still remains to be improved.

For example, I'll explain how using PBT helped me find and fix bugs in
the specification and implementation of Komposition's video
classifier. Those were bugs that would be very hard to find using
example-based tests or using a static type system!

This series is not a tutorial on PBT, but rather a collection of
motivating examples. That said, you should be able to follow along
without prior knowledge of PBT.

## Komposition

In early 2018 I started producing [Haskell
screencasts](https://haskell-at-work.com). A majority of the work
involved cutting and splicing video by hand in a [non-linear editing
system (NLE)](https://en.wikipedia.org/wiki/Non-linear_editing_system)
like [Premiere Pro](https://en.wikipedia.org/wiki/Adobe_Premiere_Pro)
or [Kdenlive](https://kdenlive.org/en/). I decided to write a
screencast editor specialized for my needs, reducing the amount of
manual labor needed to edit the recorded material. Komposition was
born.

Komposition is a modal GUI application built for editing
screencasts. Unlike most NLE systems, it features a hierarchical
timeline, built out of _sequences_, _parallels_, _tracks_, _clips_,
and _gaps_. To make the editing experience more efficient, it
automatically classifies scenes in screen capture video, and sentences
in recorded voice-over audio.

If you are curious about Komposition and want to learn more right
away, check out its
[documentation](https://owickstrom.github.io/komposition/).

![Komposition's timeline mode.](/assets/property-based-testing-the-ugly-parts/komposition-light.png){width=100%}

Some of the most complex parts of Komposition include focus and
timeline transformations, video classification, video rendering, and
the main application logic. Those are the areas in which I've spent
most effort writing tests, using a combination of example-based and
property-based testing.

I've selected the four most interesting areas where I've applied PBT
in Komposition, and I'll cover one in each coming blog post:

1. Timeline flattening
2. Video scene classification
3. Focus and timeline consistency
4. Symmetry of undo/redo

I hope these case studies will be motivating, and that they will show
the value of properties all the way from unit testing to integration
testing.

## Property-Based Testing

To get the most out of this series, you need a basic understanding of
what PBT is, so let's start there. For my take on a minimal
definition, PBT is about:

1. Specifying your system under test in terms of properties, where
   properties describe invariants of the system based on its input
   and output.
2. Testing that those properties hold against a large variety of
   inputs.

It's worth noting that PBT is not equal to QuickCheck, or any other
specific tool, for that matter. The set of inputs doesn't have to be
randomly generated. You don't have to use "shrinking". You don't have
to use a static type system or a functional programming language. PBT
is a general idea that can be applied in many ways.

The following resources are useful if you want to learn more about
PBT:

* The [introductory articles on
  Hypothesis](https://hypothesis.works/articles/intro/), although
  specific to Python.
* ["What is Property Based
  Testing?"](https://hypothesis.works/articles/what-is-property-based-testing/)
  by David R. MacIver is a definition of what PBT is, and particularly
  what it _isn't_.

The code examples will be written in Haskell and using the
[Hedgehog](https://hackage.haskell.org/package/hedgehog) testing
system. You don't have to know Haskell to follow this series, as
I'll explain the techniques primarily without code. But if you are
interested in the Haskell specifics and in Hedgehog, check out
["Property testing with
Hedgehog"](https://teh.id.au/#/posts/2017/04/23/property-testing-with-hedgehog/)
by Tim Humphries.

## Properties of the Ugly Parts

When I started with PBT, I struggled with applying it to anything
beyond simple functions. Examples online are often focused on the
fundamentals. They cover concepts like reversing lists, algebraic
laws, and symmetric encoders and decoders. Those are important
properties to test, and they are good examples for teaching the
foundations of PBT.

I wanted to take PBT beyond pure and simple functions, and leverage it
on larger parts of my system. The "ugly" parts, if you will. In my
experience, the complexity of a system often becomes much higher than
the sum of its parts. The way subsystems are connected and form a
larger graph of dependencies drives the need for integration testing
at an application level.

Finding resources on integration testing using PBT is hard, and it
might drive you to think that PBT is not suited for anything beyond
the introductory examples. With the case studies in this blog series I
hope to contribute to debunking such misconceptions.

### Designing for Testability

In my case, it's a desktop multimedia application. What if we're
working on a backend that connects to external systems and databases?
Or if we're writing a frontend application with a GUI driven by user
input? In addition to these kinds of systems being hard to test at a
high level due to their many connected subsystems, they usually have
stateful components, side effects, and non-determinism. How do we make
such systems testable with properties?

Well, the same way we would design our systems to be testable with
examples. Going back to ["Writing Testable
Code"](https://testing.googleblog.com/2008/08/by-miko-hevery-so-you-decided-to.html)
by Miško Hevery from 2008, and Kent Beck's ["Test-Driven Development
by
Example"](https://www.amazon.com/Test-Driven-Development-Kent-Beck/dp/0321146530)
from 2003, setting aside the OOP specifics, many of their guidelines
apply equally well to code tested with properties:

* **Determinism:** Make it possible to run the "system under test"
  deterministically, such that your tests can be reliable. This does
  _not_ mean the code has to be pure, but you might need to stub or
  otherwise control side effects during your tests.
* **No global state:** In order for tests to be repeatable and
  independent of execution order, you might have to rollback database
  transactions, use temporary directories for generated files, stub
  out effects, etc.
* **High cohesion:** Strive for modules of high cohesion, with smaller
  units each having a single responsibility. Spreading closely related
  responsibilities thin across multiple modules makes the
  implementation harder to maintain and test.
* **Low coupling:** Decrease coupling between interface and
  implementation. This makes it easier to write tests that don't
  depend on implementation details. You may then modify the
  implementation without modifying the corresponding tests.

I find these guidelines universal for writing testable code in any
programming language I've used professionally, regardless of paradigm
or type system. They apply to both example-based and property-based
testing.

### Patterns for Properties

Great, so we know how to write testable code. But how do we write
properties for more complex units, and even for integration testing?
There's not a lot of educational resources on this subject that I know
of, but I can recommend the following starting points:

* ["Choosing properties for property-based
  testing"](https://fsharpforfunandprofit.com/posts/property-based-testing-2/)
  by Scott Wlaschin, giving examples of properties within a set of
  common categories.
* The talk ["Property-Based Testing for Better
  Code"](https://www.youtube.com/watch?v=shngiiBfD80) by Jessica Kerr,
  with examples of generating valid inputs and dealing with timeouts.

Taking a step back, we might ask "Why it's so hard to come up with
these properties?" I'd argue that it's because doing so forces us to
understand our system in a way we're not used to. It's challenging
understanding and expressing the general behavior of a system, rather
than particular _anecdotes_ that we've observed or come up with.

If you want to get better at writing properties, the only advice I can
give you (in addition to studying whatever you can find in other
projects) is to _practice_. Try it out on whatever you're working
on. Talk to your colleagues about using properties in addition to
example-based tests at work. Begin at a smaller scale, testing simple
functions, and progress towards testing larger parts of your system
once you're comfortable. It's a long journey, filled with reward,
surprise, and joy!

## Testing Case Studies

With a basic understanding of PBT, how we can write testable code, and
how to write properties for our system under test, we're getting ready
to dive into the case studies:

1. **Introduction**
1. [Timeline Flattening](/2019-03-24-property-based-testing-in-a-screencast-editor-case-study-1.html)
1. [Video Scene Classification](/2019-04-17-property-based-testing-in-a-screencast-editor-case-study-2.html)
1. [Integration Testing](/2019-06-02-property-based-testing-in-a-screencast-editor-case-study-3.html)

## Credits

Thank you Chris Ford, Alejandro Serrano Mena, Tobias Pflug, Hillel
Wayne, and Ulrik Sandberg for kindly providing your feedback on my
drafts!

## Buy the Book

This series is now available as an [ebook on
Leanpub](https://leanpub.com/property-based-testing-in-a-screencast-editor). While
the content is mostly the same, there are few changes bringing it
up-to-date. Also, if you've already enjoyed the articles, you might
want support my work by purchasing this book. Finally, you might enjoy
a nicely typeset PDF, or an EPUB book, over a web page.
