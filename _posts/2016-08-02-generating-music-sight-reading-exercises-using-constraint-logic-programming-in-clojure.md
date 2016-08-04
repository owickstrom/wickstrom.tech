---
layout: post
title: "Generating Music Sight Reading Exercises using Constraint Logic Programming in Clojure"
date: 2016-08-03 08:00 +0200
published: false
author: Oskar Wickström
categories: Programming
tags: ["music", "logic", "clp", "clojure"]
---

Programming is fun! Music is fun! Combining the two seems like a joyful
endeavour. I have long been curious about generating music as a tool for
practicing sight-reading. It does not have to be beautiful music, its
purpose is to help practitioners advance their skills gradually. I do, however,
want to explore ways to generate music following patterns and idioms, resulting
in realistic and useful material.

## Defining the Musical Model

Music theory is a huge area with lots of rules and exceptions. Trying to
include all those rules in our initial attempts at a music generator would not
be productive. Let us instead define the model for our program by picking a
subset of the constructs and rules from music theory, and incrementally expand
that model to meet our needs.

Our first step is to define a goal, a piece of music that exhibits the elements
and patterns we want in a generated sight-reading exercice. Score 1.1 is a
four-measure melody I wrote by hand. In my personal taste this melody does not
sound random or generated, it is musical.

{% lilypond Score 1.1: A hand-written melodious sight reading exercise. %}
\relative {
    c''4 b a g
    a2 r4 e8 g
    e4 r8 d16 c d8. c16  a8 g
    a2 r2
}
{% endlilypond %}

Let's have a closer look at the music in Score 1.1. This section will include
only the minimum amount of music theory needed to understand the rest of the
article. If you are interested in digging deeper, I encourage you to check out
[Music Theory for Musicians and Normal People](http://tobyrush.com/theorypages/)
for a light introduction to music theory.

<p class="draft">
We have a score in C major, something about modes, G clef, bla bla bla... Maybe
something about this article not including modes in the program.
</p>

The score consists of four measures, also called *bars*, divided by bar
lines. The time signature is 4/4, as denoted by the common time
sign (<img src="/assets/music/common-time.1x.png"
           srcset="/assets/music/common-time.2x.png 2x, /assets/music/common-time.1x.png 1x"
           alt="Common time sign"
           style="margin: 0 .25em;">).
With a time signature of 4/4,
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
<td>
<img src="/assets/music/half.1x.png"
     srcset="/assets/music/half.2x.png 2x, /assets/music/half.1x.png 1x"
     alt="Half note">
</td>
<td>Half note</td>
<td>1/2</td>
</tr>
<tr>
<td>
<img src="/assets/music/half.1x.png"
     srcset="/assets/music/quarter.2x.png 2x, /assets/music/quarter.1x.png 1x"
     alt="Quarter note">
</td>
<td>Quarter note</td>
<td>1/4</td>
</tr>
<tr>
<td>
<img src="/assets/music/half.1x.png"
     srcset="/assets/music/eighth.2x.png 2x, /assets/music/eighth.1x.png 1x"
     alt="Eighth note">
</td>
<td>Eighth note</td>
<td>1/8</td>
</tr>
<tr>
<td>
<img src="/assets/music/sixteenth.1x.png"
     srcset="/assets/music/sixteenth.2x.png 2x, /assets/music/sixteenth.1x.png 1x"
     alt="Sixteenth note">
</td>
<td>Sixteenth note</td>
<td>1/16</td>
</tr>
<tr>
<td>
<img src="/assets/music/dotted-eighth.1x.png"
     srcset="/assets/music/dotted-eighth.2x.png 2x, /assets/music/dotted-eighth.1x.png 1x"
     alt="Dotted eighth note">
</td>
<td>Dotted eighth note</td>
<td>3/16</td>
</tr>
<tr>
<td>
<img src="/assets/music/half-rest.1x.png"
     srcset="/assets/music/half-rest.2x.png 2x, /assets/music/half-rest.1x.png 1x"
     alt="Dotted eighth note">
</td>
<td>Half rest</td>
<td>1/2</td>
</tr>
<tr>
<td>
<img src="/assets/music/quarter-rest.1x.png"
     srcset="/assets/music/quarter-rest.2x.png 2x, /assets/music/quarter-rest.1x.png 1x"
     alt="Dotted eighth note">
</td>
<td>Quarter rest</td>
<td>1/4</td>
</tr>
<tr>
<td>
<img src="/assets/music/eighth-rest.1x.png"
     srcset="/assets/music/eighth-rest.2x.png 2x, /assets/music/eighth-rest.1x.png 1x"
     alt="Dotted eighth note">
</td>
<td>Eighth rest</td>
<td>1/4</td>
</tr>
</tbody>
</table>

<div class="caption">Musical elements used in Score 1.1.</div>

The note values in the conventional Western music System are *dyadic rational
numbers*, i.e. rational numbers where the denominator is a power of two. Dotted
notes are 1,5 times as long as their original notes, and are used to succinctly
denote durations *in between* the duration of non-dotted notes. This is similar
to how inch units are divided in the imperial system.

<figure>
<img alt="Dyadic rational sub-divisions"
     src="/assets/dyadic-rational-subdivisions.svg"/>
<figcaption>
Dyadic rational divisions. Graphic from
<a href="https://en.wikipedia.org/wiki/File:Dyadic_rational.svg">Wikipedia</a>.
</figcaption>
</figure>

Another way of denoting such durations is by *tying* multiple notes together.
The following two scores have the same musical meaning, in that they sound the
same way when played, but Score 1.2 is notated using ties, and Score 1.3
is notated using dotted notes.

{% lilypond Score 1.2: Using ties for 3/8 and 3/16 duration notes. %}
\relative {
    c'4~c8 c8~c4 r4 c8~ c16 c16~ c8 r8 r2
}
{% endlilypond %}

{% lilypond Score 1.3: Using dotted notes for 3/8 and 3/16 duration notes. %}
\relative {
    c'4. c r4 c8. c r8 r2
}
{% endlilypond %}

Another important aspect of musical notation is *note grouping*. This technique
helps the reader to see the sub-divisions of a bar by visually grouping notes.
Notes are commonly grouped within quarter and eighth note durations, depending
on the note values. Score 1.4. shows a phrase of quarter notes following a
single sixteenth note, without any grouping. The same phrase is written with
note groups in Score 1.5 for greater readability.

{% lilypond Score 1.4. A phrase without proper grouping is harder to read. %}
\relative {
    c'16 d4 e4 f4 g16 r8
}
{% endlilypond %}

{% lilypond Score 1.5: Quarter notes spanning groups are split into dotted eighth notes and sixteenth notes, and tied together. %}
\relative {
    c'16 d8.~d16 e8.~e16 f8.~f16 g16 r8
}
{% endlilypond %}

Exceptions are made for common rythmic patterns, like the eighth note followed
by a quarter note and an eighth note (see Score 1.6). Multiple quarter notes
following an eighth note should be grouped, as shown in Score 1.7.

{% lilypond Score 1.6: A common rythmic pattern that does not need grouping. %}
\relative {
    c'8 d4 e8 r4 r2
}
{% endlilypond %}

{% lilypond Score 1.7: Multiple quarter notes broken up into tied eighth notes. %}
\relative {
    c'8 d8~d8 e8~e8 f8~f8 g8
}
{% endlilypond %}

We have not talked about key signatures, accidentals, scales, chords, and many
other central concepts in musical notation. This is however enough of a model
to start generating simple sight-reading exercises. I will start from the
simplest possible model and gradually add constraints to make the generated
music more "realistic" and challenging.

## A Naive Generator

<p class="draft">
About building the first generator. No difficulty settings, no groupings.
Should explain core.logic and why we get permutations in order. How to do
random? Do we need random?
</p>

## Note Grouping

<p class="draft">
Extend the first generator with note groups.
</p>

## Difficulty Levels

<p class="draft">
Extend the first generator with difficulty setting.
</p>

## Next Steps

<p class="draft">
What have we covered? What can be the next steps? Link to a complete sample on
GitHub.
</p>
