---
layout: post
title: "Generating Music Sight Reading Exercises using Constraint Logic Programming in Clojure"
date: 2016-08-03 08:00 +0200
published: false
author: Oskar Wickström
categories: Programming
tags: ["music", "logic", "clp", "clojure"]
---

Foo bar baz.

{% lilypond A short example of a hand-written sight reading exercise. %}
\relative {
  c''4 b a g
  a2 r4 e8 g
  e4 r8 d16 c d8. c16  a8 g
  c2 r2 \bar "|."
}
{% endlilypond %}

Let's try to break this down...

## A Naive Generator

...
