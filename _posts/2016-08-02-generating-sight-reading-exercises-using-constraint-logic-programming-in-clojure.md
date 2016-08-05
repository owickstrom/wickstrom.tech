---
layout: post
title: "Generating Sight-Reading Exercises using Constraint Logic Programming in Clojure"
date: 2016-08-03 08:00 +0200
published: false
author: Oskar Wickström
categories: Programming
tags: ["music", "logic", "clp", "clojure"]
---

Programming is fun! Music is fun! Combining the two should be a joyful
endeavour. I have long been curious about generating music as a tool for
practicing sight-reading. It does not have to be beautiful music, its purpose
is to help practitioners advance their skills gradually. I do, however, want to
explore ways to generate music following patterns and idioms, resulting in
realistic and useful material.

This article introduces concepts both from music theory and from constraint
logic programming using [core.logic][1]. We will build a naive music generator
based on a simplified musical model, and incrementally improve the output by
adding constraints from our model.

[1]: https://github.com/clojure/core.logic

## Defining the Musical Model

Music theory is a huge area with lots of rules and exceptions. Trying to
include all those rules in our initial attempts at a music generator would not
be productive. Let us instead define the model for our program by picking a
subset of the constructs and rules from music theory, and incrementally expand
that model to meet our needs.

Our first step is to define a goal, a piece of music that exhibits the elements
and patterns we want in a generated sight-reading exercise. Score 1.1 is a
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

Let's have a closer look at the music in Score 1.1. I will cover only the
minimum amount of music theory needed to understand the rest of the
article. If you are interested in digging deeper, I encourage you to check out
[Music Theory for Musicians and Normal People][1]
for a light introduction to music theory.

The music in Score 1.1 is in the key of C major, or A minor. A key
signature is a set of sharp
(<img src="/assets/music/sharp.1x.png"
      srcset="/assets/music/sharp.2x.png 2x, /assets/music/sharp.1x.png 1x"
      alt="Sharp sign"
      style="margin: 0 .25em;">)
or flat
(<img src="/assets/music/flat.1x.png"
      srcset="/assets/music/flat.2x.png 2x, /assets/music/flat.1x.png 1x"
      alt="Flat sign"
      style="margin: 0 .25em;">)
symbols, raising or lowering the note on the line it is placed, until the next
key signature, or to the end of the score. A sharp raises the note by a
semitone, a flat lowers the note by a semitone. In our example we have no sharp
or flat symbols in the key signature, thus all notes are in the diatonic C
scale. Score 1.2 shows a key signature for music in the key of D major, raising
all F notes to F#, and all C notes to C#.

{% lilypond Score 1.2: Music in the key of D major. %}
\relative {
  \key d \major
  d'8 e fis g b a cis b
  d4 r4 r2
}
{% endlilypond %}

Please see [Circle of fifths][2] for more information on key signatures, major
and minor keys, and diatonic scales. For the purpose of this article you will
only need to be aware that keys exist and that key signatures affect the notes
to be played in a score.

Score 1.1 consists of four measures, also called *bars*, divided by bar
lines. The time signature is 4/4, as denoted by the common time
sign
(<img src="/assets/music/common-time.1x.png"
      srcset="/assets/music/common-time.2x.png 2x, /assets/music/common-time.1x.png 1x"
      alt="Common time sign"
      style="margin: 0 .25em;">).
With a time signature of 4/4, the total value of notes in each bar must
equal 4/4. It might be tempting to say that the total value of a bar must be
1 at all times, but it is not that simple. In other time signatures, such as
3/4, 6/8, and 5/4, the total is not 1.

The following table describes the used note symbols and their meaning. It is in
no way a complete list of musical symbols.

<table class="musical-signs">
<thead>
<tr>
<th>Symbol</th>
<th>Value</th>
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
<td>1/8</td>
</tr>
</tbody>
</table>

<div class="caption">Musical elements used in Score 1.1.</div>

A note describes both the pitch of a sound, and the relative duration of the
sound. The vertical position in the staff, along with key signature and
accidentals, determines the pitch. The *note value*, written using differents
note head shapes, stem, and beams, determines the relative duration.

The note values in the conventional Western music system are *dyadic rational
numbers*, i.e. rational numbers where the denominator is a power of two. Note
values can be modified using dots. A dot adds the next shorter note value to
the original note value, effectively making it one and half times longer. The
technique of using dotted notes is similar to how inch units are divided in the
imperial system.

<figure>
<img alt="Dyadic rational sub-divisions"
     src="/assets/dyadic-rational-subdivisions.svg"/>
<figcaption>
Dyadic rational divisions. Graphic from
<a href="https://en.wikipedia.org/wiki/File:Dyadic_rational.svg">Wikipedia</a>.
</figcaption>
</figure>

Another way of describing such note values is by *tying* multiple notes together.
The following two scores have the same musical meaning, in that they sound the
same way, but Score 1.3 is notated using ties, and Score 1.4 is notated using
dotted notes.

{% lilypond Score 1.3: Using ties for 3/8 and 3/16 duration notes. %}
\relative {
    c'4~c8 c8~c4 r4 c8~ c16 c16~ c8 r8 r2
}
{% endlilypond %}

{% lilypond Score 1.4: Using dotted notes for 3/8 and 3/16 duration notes. %}
\relative {
    c'4. c r4 c8. c r8 r2
}
{% endlilypond %}

Another important aspect of musical notation is *note grouping*. This technique
helps the reader to see the sub-divisions of a bar by visually grouping notes.
Notes are commonly grouped within quarter and eighth note durations, depending
on the note values. Score 1.5. shows a phrase of quarter notes following a
single sixteenth note, without any grouping. The same phrase is written with
note groups in Score 1.6 for greater readability.

{% lilypond Score 1.5. A phrase without proper grouping is harder to read. %}
\relative {
    c'16 d4 e4 f4 g16 r8
}
{% endlilypond %}

{% lilypond Score 1.6: Quarter notes spanning groups are split into dotted eighth notes and sixteenth notes, and tied together. %}
\relative {
    c'16 d8.~d16 e8.~e16 f8.~f16 g16 r8
}
{% endlilypond %}

Exceptions are made for common rythmic patterns, like the eighth note followed
by a *single* quarter note and an eighth note (see Score 1.7). Multiple quarter
notes following an eighth note should be grouped, as shown in Score 1.8.

{% lilypond Score 1.7: A common rythmic pattern that does not need grouping. %}
\relative {
    c'8 d4 e8 r2
}
{% endlilypond %}

{% lilypond Score 1.8: Multiple quarter notes broken up into tied eighth notes. %}
\relative {
    c'8 d8~d8 e8~e8 f8~f8 g8
}
{% endlilypond %}

We have just scratched the surface of music theory in describing the first
piece of music, but we have enough of a model to start generating simple
sight-reading exercises. Let us start by building a simple program using
*core.logic*, and then gradually add constraints to make the generated music
more "realistic" and challenging.

[1]: http://tobyrush.com/theorypages/
[2]: https://en.wikipedia.org/wiki/Circle_of_fifths

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

## Varying Pitch and Duration

<p class="draft">
Add constraints to enforce variation, in pitch and duration.
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
