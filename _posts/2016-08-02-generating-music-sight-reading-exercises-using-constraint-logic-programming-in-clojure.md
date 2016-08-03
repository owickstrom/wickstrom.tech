---
layout: post
title: "Generating Music Sight Reading Exercises using Constraint Logic Programming in Clojure"
date: 2016-08-03 08:00 +0200
published: false
author: Oskar Wickström
categories: Programming
tags: ["music", "logic", "clp", "clojure"]
---

Programming is fun. Music is fun. Combining the two seems like a joyful
endeavour. I have long been curious about generating music as a tool for
practicing *sight-reading*. It does not have to be beautiful music, its
purpose is to help practitioners advance their skills in sight-reading.
I do, however, want to explore ways to generate music following certain
patterns and idioms, resulting in realistic and useful material.

The following example is a four-measure melody I wrote by hand, including
some of the elements I want a music-generating program to produce. In
my personal taste this melody does not sound random or generated, it is
musical.

{% lilypond A hand-written melodious sight reading exercise. %}
\relative {
  c''4 b a g
  a2 r4 e8 g
  e4 r8 d16 c d8. c16  a8 g
  a2 r2 \bar "|."
}
{% endlilypond %}

Let's have a closer look at the melody, its musical elements, and the
idioms used. We have a score consisting of four measures, also called *bars*, in
the key of C major. The time signature is 4/4, as denoted by the *common time*
sign (<span class="music-sign">&#119092;</span>). With a time signature of 4/4,
the total duration of notes in each bar must equal 4/4. It might be tempting to
say that the total duration of a bar must be 1 at all times, but it is not that
simple. In other time signatures, such as 3/4, 6/8, and 5/4, the total is not 1.

The following table describes the used note symbols and their meaning. It is in
no way a complete list of musical symbols.

<table class="musical-signs">
<thead>
<tr>
<th>Symbol</th>
<th>Name</th>
<th>Duration</th>
</tr>
</thead>
<tbody>
<tr>
<td>&#119134;</td>
<td>Half note</td>
<td>1/2</td>
</tr>
<tr>
<td>&#119135;</td>
<td>Quarter note</td>
<td>1/4</td>
</tr>
<tr>
<td>&#119136;</td>
<td>Eighth note</td>
<td>1/8</td>
</tr>
<tr>
<td>&#119136;</td>
<td>Sixteenth note</td>
<td>1/16</td>
</tr>
<tr>
<td>&#119136; &#119149;</td>
<td>Dotted eighth note</td>
<td>3/16</td>
</tr>
<tr>
<td>&#119100;</td>
<td>Half rest</td>
<td>1/2</td>
</tr>
<tr>
<td>&#119101;</td>
<td>Quarter rest</td>
<td>1/4</td>
</tr>
<tr>
<td>&#119102;</td>
<td>Eighth rest</td>
<td>1/4</td>
</tr>
</tbody>
</table>

<div class="caption">The musical elements used in the previous score.</div>

The note values in the conventional Western music System are *dyadic rational
numbers*, i.e. rational numbers where the denominator is a power of two. Dotted
notes are 1,5 times as long as their original notes, and are used to succinctly
denote durations *in between* the duration of non-dotted notes. You can compare
this with sub-divisions of inches in the imperial system.

<figure>
<img alt="Dyadic rational sub-divisions"
     src="/assets/dyadic-rational-subdivisions.svg"/>
<figcaption>
Dyadic rational divisions. Graphic from
<a href="https://en.wikipedia.org/wiki/File:Dyadic_rational.svg">Wikipedia</a>.
</figcaption>
</figure>

Another way of denoting such durations is by *tying* multiple notes together.
The following two examples have the same musical meaning, but the first is
notated using ties, and the second using dotted notes.

{% lilypond Using ties for 3/8 and 3/16 duration notes. %}
\relative {
  c'4~c8 c8~c4 r4 c8~ c16 c16~ c8 r8 r2
}
{% endlilypond %}

{% lilypond Using dotted notes for 3/8 and 3/16 duration notes. %}
\relative {
  c'4. c r4 c8. c r8 r2
}
{% endlilypond %}

## A Naive Generator

...
