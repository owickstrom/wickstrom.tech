---
title: Projects
layout: page
---

These are some of the projects I am, or have been, working on. All open-source
programming projects listed below, and many more, are available at [my GitHub
profile page](https://github.com/owickstrom).

## Property-Based Testing in a Screencast Editor

<a href="https://leanpub.com/property-based-testing-in-a-screencast-editor"
   style="float: right; width: 200px; margin: 0 0 0 2em; display: block;">
    <img src="/assets/property-testing-short-book-cover.jpg"
         alt="Book cover for Property-Based Testing in a Screencast Editor"
         style="display: block; margin: 0;">
</a>

[Property-Based Testing in a Screencast
Editor](https://leanpub.com/property-based-testing-in-a-screencast-editor)
is a short book on using property-based testing (PBT) within
Komposition (described below). It's based on articles published on
[this site](/writing.html). While the content is mostly the same,
there are few changes bringing it up-to-date. Also, if you've already
enjoyed the articles, you might want support my work by purchasing
this book ($10). Finally, you might enjoy a nicely typeset PDF, or an
EPUB book, over a web page.

## Komposition

[Komposition](https://owickstrom.github.io/komposition/) is the video editor
built for screencasters. It lets you focus on producing and publishing quality
content, instead of spending all of your time in complicated video editors.
Stop wasting time on manually adjusting clip lengths, building still-frame
segments, and dragging clips around, and enjoy a new screencast editing
experience.

Komposition is written in GHC Haskell using [GTK](https://www.gtk.org/),
[GStreamer](https://gstreamer.freedesktop.org/), and
[FFmpeg](https://ffmpeg.org/).

## Haskell at Work Screencasts

[Haskell at Work](https://haskell-at-work.com/) is a screencast focused on
practical Haskell programming. Viewers should have a basic understanding of
Haskell, and be eager to learn new ways of working with Haskell. Check out the
site and subscribe to the YouTube channel if you want to learn more about
Haskell in the wild!

## Hyper

The goal of [Hyper](https://hyper.wickstrom.tech) is to make use of row
polymorphism, and other tasty type system features in [PureScript], to enforce
correctly stacked middleware in HTTP server applications. All effects of
middleware should be reflected in the types to ensure that otherwise common
mistakes cannot be made.

## PureScript Spec

[PureScript Spec](https://purescript-spec.github.io/purescript-spec/) is a testing
framework for PureScript. I started building it April, 2015, but nowadays
it's maintained by others. It's inspired by the Haskell testing framework
[hspec](https://hspec.github.io/).

## The Oden Programming Language

As described at [oden-lang.github.io], *Oden is an experimental,
statically typed, functional programming language, built for the Go
ecosystem.* I worked on the language for the majority of 2016, rounding off
in October. Why I stopped working on Oden is explained in greater detail in
the blog post [Taking a Step Back from
Oden](/programming/2016/10/10/taking-a-step-back-from-oden.html).

## DataFlow

During my work at Sony Mobile, I created [DataFlow], a tool that renders graphs
using a declarative markup. It is built around the [DFD] format, but also
supports sequence diagrams and structured data output. We used it to document
our service integrations and security requirements between separate systems,
and integrated it in to our bigger documentation workflow.

[PureScript]: https://www.purescript.org/
[oden-lang.github.io]: https://oden-lang.github.io
[DataFlow]: https://github.com/sonyxperiadev/dataflow
[DFD]: https://en.wikipedia.org/wiki/Data_flow_diagram
