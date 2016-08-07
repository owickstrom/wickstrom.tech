---
layout: post
title: "Generating Sight-Reading Exercises using Constraint Logic Programming in Clojure, Part 1"
date: 2016-08-03 08:00 +0200
published: false
author: Oskar Wickström
categories: Programming
tags: ["music", "logic", "clp", "clojure"]
excerpt: "This is the first post in a series about generating sheet music for sight-reading exercises, using Clojure and core.logic."
---

_This is the first post in a series about generating sheet music for
sight-reading exercises, using Clojure and core.logic. The size and direction
of the series is not set in stone; your feedback matters! Please post a comment
or get in touch on [Twitter](twitter)._

[twitter]: https://twitter.com/owickstrom

## Introduction

Programming is fun! Music is fun! Combining the two should be a joyful
endeavour. I have long been curious about generating music as a tool for
practicing sight-reading. It does not have to be beautiful music, its purpose
is to help practitioners advance their skills gradually. I do, however, want to
explore ways to generate music following patterns and idioms, resulting in
realistic and useful material.

### What You Need to Know

This post introduces concepts both from music theory and from constraint
logic programming using [core.logic][core-logic]. You are not expected to have
any knowledge about music theory. Some familiarity with core.logic, or logic
programming languages like Prolog or miniKanren, is needed. I recommend you to
go through [the core.logic primer][core-logic-primer] for an introduction.

[core-logic]: https://github.com/clojure/core.logic
[core-logic-primer]: https://github.com/clojure/core.logic/wiki/A-Core.logic-Primer

## Defining the Musical Model

Music theory is a huge area with lots of rules and exceptions. Trying to
include all those rules in our initial attempts at a music generator would not
be productive. Let's instead define the model for our program by picking a
subset of the constructs and rules from music theory, and incrementally expand
that model to meet our needs.

Our first step is to define the goals of our project. I have asked my musician
friends on Facebook what they think is the most important traits of a
sight-reading exercise. The following is a list based on my own evaluation, and
their responses, in order of importance.

1. Rythmic variation
1. Pitch variation
1. Common rythmic patterns
1. Rests
1. Interesting pitch variations (melodies, scales, patterns)
1. Key signatures, modes, accidentals
1. Parameterized difficulty
1. Playback (generate a MIDI or WAV file)
1. Odd time signatures

In this post we will work with points 1-3. I hope this series will be able to
cover most or all of the aspects in the list.

As a complement to the prioritized list, we will look at a piece of music that
exhibits the elements and patterns we want in a generated sight-reading
exercise. [Score 1](#score-1) is a four-measure melody I wrote by hand. In my
personal taste this melody does not sound random or generated, it is musical.
We will use it as an inspiration and long-term goal for our music-generator
project.

{% lilypond A hand-written melodious sight reading exercise. %}
\relative {
    c''4 b a g
    a2 r4 e8 g
    e4 r8 d16 c d8. c16  a8 g
    a2 r2
}
{% endlilypond %}

Let's have a closer look at the music in [Score 1](#score-1). I will cover only the
minimum amount of music theory needed to understand the rest of the
post. If you are interested in digging deeper, I encourage you to check out
[Music Theory for Musicians and Normal People][1]
for a light introduction to music theory.

The music in [Score 1](#score-1) is in the key of C major, or A minor. A key
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
scale. [Score 2](#score-2) shows a key signature for music in the key of D major, raising
all F notes to F#, and all C notes to C#.

{% lilypond Music in the key of D major. %}
\relative {
  \key d \major
  d'8 e fis g b a cis b
  d4 r4 r2
}
{% endlilypond %}

Please see [Circle of fifths][2] for more information on key signatures, major
and minor keys, and diatonic scales. For the purpose of this post you will
only need to be aware that keys exist and that key signatures affect the notes
to be played in a score.

[Score 1](#score-1) consists of four measures, also called *bars*, divided by bar
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

<div class="caption">Notes, and their symbols, used in <a href="#score-1">Score 1</a>.</div>

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
same way, but [Score 3](#score-3) is notated using ties, and [Score 4](#score-4) is notated using
dotted notes.

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

Another important aspect of musical notation is *note grouping*. This technique
helps the reader to see the sub-divisions of a bar by visually grouping notes.
Notes are commonly grouped within quarter and eighth note durations, depending
on the note values. [Score 5](#score-5). shows a phrase of quarter notes following a
single sixteenth note, without any grouping. The same phrase is written with
note groups in [Score 6](#score-6) for greater readability.

{% lilypond A phrase without proper grouping is harder to read. %}
\relative {
    c'16 d4 e4 f4 g16 r8
}
{% endlilypond %}

{% lilypond Quarter notes spanning groups are split into dotted eighth notes and sixteenth notes, and tied together. %}
\relative {
    c'16 d8.~d16 e8.~e16 f8.~f16 g16 r8
}
{% endlilypond %}

Exceptions are made for common rythmic patterns, like the eighth note followed
by a *single* quarter note and an eighth note, illustrated in [Score
7](#score-7). Multiple quarter notes following an eighth note should be
grouped, as shown in [Score 8](#score-8).

{% lilypond A common rythmic pattern that does not need grouping. %}
\relative {
    c'8 d4 e8 r2
}
{% endlilypond %}

{% lilypond Multiple quarter notes broken up into tied eighth notes. %}
\relative {
    c'8 d8~d8 e8~e8 f8~f8 g8
}
{% endlilypond %}

We have just scratched the surface of music theory in describing the first
piece of music, but we have enough of a model to start generating simple
sight-reading exercises. Let's start by building a simple program using
*core.logic*, and then gradually add constraints to make the generated music
more realistic and challenging.

[1]: http://tobyrush.com/theorypages/
[2]: https://en.wikipedia.org/wiki/Circle_of_fifths

## A Naive Generator

We start out building our first naive generator by only encoding very basic
properties of notes and bars. As we are generating sheet music, let's call this
project *SMUG*, short for <u>S</u>heet <u>Mu</u>sic <u>G</u>enerator. We begin
by declaring our namespace and requiring `core.typed` and `core.typed.fd`. The
`fd` namespace contains the stuff we need to work with finite domains.

```clojure
(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))
```

A notes has a pitch and a value. We represent the pitch an integer between 1
and 7, inclusive. To use finite domain constraints we need to find ways of
representing the values in our domain as integers. We will map the results of
queries to other data types later. Using `interval` we create a finite domain
based on the of numbers, and constrain p to that domain.

```clojure
(defn pitcho [p]
  (fd/in p (fd/interval 1 7)))
```

Similarly to note pitch, we represent the note value as an integer. In this
case, however, the note value is not within a range of numbers, but a number in
the set `{1, 2, 4, 8, 16}` , representing the numerators in 1/16, 2/16, 4/16,
8/16, and 16/16.  In other words, with this representation we only support the
note values from whole notes down to sixteenth notes. Using `domain` we create
a finite domain based on the set of numbers, and constrain `v` to that domain.

```clojure
(defn note-valueo [v]
  (fd/in v (fd/domain 1 2 4 8 16)))
```

Let's compose these two relations into a note relation, with the pair of pitch
and note value as a vector. We use `defne` to define a relation that
destructures note into its parts `p` and `v`, and then constrain the pitch and
note value.

```clojure
(defne noteo [note]
  ([ [p v] ]
   (pitcho p)
   (note-valueo v)))
```

A bar can be represented as a sequence of notes. Using our `noteo` relation, we
recursively describe the sequence. The first branch matches the empty sequence
and succeeds. The second branch matches the non-empty sequence, constrains the
first element to be a note, and recurses on the rest of the sequence.

```clojure
(defne noteso [notes]
  ([ [] ])
  ([ [n . ns] ]
   (noteo n)
   (noteso ns)))
```

We are soon ready to define `baro`, but first we need a way to ensure that the
note values add up to a total of 16. To make the generator support different
time signatures we would have to make that number configurable. For now we will
only generate music in 4/4, so hard-coding 16 is fine.

For empty sequences the total is zero. For non-empty sequences we ensure that
`v` is a note value and unify `total` with `s`, where `s` is the sum of `v`
and the total of the remaining notes.

```clojure
(defne notes-total-valueo [notes total]
  ([ [] _ ]
   (fd/== total 0))
  ([ [[p v] . ns] _ ]
   (fresh (s)
     (note-valueo v)
     (fd/+ v s total)
     (notes-total-valueo ns s))))
```

This definition is analogous to how `(reduce + (map first notes))` would work
on a sequence `notes` using regular Clojure data structures, but as we are
using logic variables we can't use `reduce` and `first`; we have to describe
the logical relation of the reduction.

Now to last relation we need to define, called `baro`. It simply states that
a bar is a sequence of notes and that the total note value must be 16.

```clojure
(defn baro [notes]
  (fresh []
    (noteso notes)
    (notes-total-valueo notes 16)))
```

We use `run` to query for bars. The following query gives us 32 valid bars,
neatly pretty-printed in the REPL.

```clojure
smug.music> (clojure.pprint/pprint
             (run 32 [q]
               (baro q)))
;; output:
(([1 16])
 ([2 16])
 ([3 16])
 ([4 16])
 ([5 16])
 ([6 16])
 ([1 8] [1 8])
 ([7 16])
 ([2 8] [1 8])
 ([3 8] [1 8])
 ([4 8] [1 8])
 ([5 8] [1 8])
 ([6 8] [1 8])
 ([7 8] [1 8])
 ([1 4] [1 4] [1 8])
 ([1 8] [1 4] [1 4])
 ([2 4] [1 4] [1 8])
 ([1 2] [1 8] [1 2] [1 4])
 ([1 4] [1 8] [1 2] [1 2])
 ([2 8] [1 4] [1 4])
 ([3 4] [1 4] [1 8])
 ([2 2] [1 8] [1 2] [1 4])
 ([1 8] [1 4] [1 2] [1 2])
 ([3 8] [1 4] [1 4])
 ([4 4] [1 4] [1 8])
 ([2 4] [1 8] [1 2] [1 2])
 ([1 1] [1 8] [1 4] [1 2] [1 1])
 ([4 8] [1 4] [1 4])
 ([3 2] [1 8] [1 2] [1 4])
 ([2 8] [1 4] [1 2] [1 2])
 ([5 4] [1 4] [1 8])
 ([2 1] [1 8] [1 4] [1 2] [1 1]))
nil
```

The output is a bit crude; a sequence of sequences of pairs of integers. Let's
define some conversion functions to make the results reflect the domain of
musical notation.

```clojure
(defn ->pitch [p]
  (nth [:c :d :e :f :g :a :b] (- p 1)))

(defn ->note-value [d]
  (/ d 16))

(defn ->note [[p d]]
  [(->pitch p)
   (->note-value d)])

(defn ->bar [bar]
  (map ->note bar))
```

All right, let's try again.

```clojure
smug.music> (clojure.pprint/pprint
             (map ->bar (run 32 [q]
                          (baro q))))
;; output:
(([:c 1])
 ([:d 1])
 ([:e 1])
 ([:f 1])
 ([:g 1])
 ([:a 1])
 ([:c 1/2] [:c 1/2])
 ([:b 1])
 ([:d 1/2] [:c 1/2])
 ([:e 1/2] [:c 1/2])
 ([:f 1/2] [:c 1/2])
 ([:g 1/2] [:c 1/2])
 ([:a 1/2] [:c 1/2])
 ([:b 1/2] [:c 1/2])
 ([:c 1/4] [:c 1/4] [:c 1/2])
 ([:c 1/2] [:c 1/4] [:c 1/4])
 ([:d 1/4] [:c 1/4] [:c 1/2])
 ([:c 1/8] [:c 1/8] [:c 1/4] [:c 1/2])
 ([:d 1/2] [:c 1/4] [:c 1/4])
 ([:c 1/4] [:c 1/8] [:c 1/8] [:c 1/2])
 ([:e 1/4] [:c 1/4] [:c 1/2])
 ([:d 1/8] [:c 1/8] [:c 1/4] [:c 1/2])
 ([:e 1/2] [:c 1/4] [:c 1/4])
 ([:c 1/2] [:c 1/8] [:c 1/8] [:c 1/4])
 ([:f 1/4] [:c 1/4] [:c 1/2])
 ([:d 1/4] [:c 1/8] [:c 1/8] [:c 1/2])
 ([:f 1/2] [:c 1/4] [:c 1/4])
 ([:c 1/16] [:c 1/8] [:c 1/16] [:c 1/2] [:c 1/4])
 ([:g 1/4] [:c 1/4] [:c 1/2])
 ([:e 1/8] [:c 1/8] [:c 1/4] [:c 1/2])
 ([:d 1/2] [:c 1/8] [:c 1/8] [:c 1/4])
 ([:g 1/2] [:c 1/4] [:c 1/4]))
```

Neat! Let's wrap all this up in to function that we can use as the API for the
generator. Here we wrap the sequence of bars in map as well. Later on we can
add other key-value pairs to the map, like the time and key signatures.

```clojure
(defn generate-score [n]
  (let [bars (run n [q]
               (baro q))]
    {:bars (map ->bar bars)}))
```

We now have a very simple, but working generator. The average musician
does not read music in the form of Clojure source code, though. We need
rendering. I stumbled across [Lilypond][lilypond], a music engraving program in
the [GNU Project][gnu-project]. The input format is a TeX-like markup that is
simple to generate, and the program can output stunningly beautiful scores in
PDF, PNG, and SVG formats. To keep the post focused, I will not include the
rendering code, but you can check out [the full source on
GitHub][github-project] if you are interested. [Score 9](#score-9) shows our
previous result rendered with Lilypond.

{% lilypond Our generated 32 bars of music. %}
{
  c'1 \bar "|" d'1 \bar "|" e'1 \bar "|" f'1 \bar "|" g'1 \bar "|" a'1 \bar "|" c'2 c'2 \bar "|" b'1 \break
   d'2 c'2 \bar "|" e'2 c'2 \bar "|" f'2 c'2 \bar "|" g'2 c'2 \bar "|" a'2 c'2 \bar "|" b'2 c'2 \bar "|" c'4 c'4 c'2 \bar "|" c'2 c'4 c'4 \break
   d'4 c'4 c'2 \bar "|" c'8 c'2 c'8 c'4 \bar "|" d'2 c'4 c'4 \bar "|" e'4 c'4 c'2 \bar "|" e'2 c'4 c'4 \bar "|" c'16 c'2 c'8 c'4 c'16 \bar "|" f'4 c'4 c'2 \bar "|" c'4 c'2 c'8 c'8 \break
   f'2 c'4 c'4 \bar "|" d'16 c'2 c'8 c'4 c'16 \bar "|" g'4 c'4 c'2 \bar "|" c'8 c'2 c'16 c'4 c'16 \bar "|" g'2 c'4 c'4 \bar "|" a'4 c'4 c'2 \bar "|" c'4 c'2 c'16 c'8 c'16 \bar "|" d'8 c'2 c'8 c'4 \bar "|."
}
{% endlilypond %}

Remember note grouping from the music theory introduction? When we render the
generated music, like in [Score 9](#score-9), the lack of proper note grouping
becomes very clear. Bar 22, 26, and 28 have intolerable sequences of notes values
without grouping. Let's fix that!

[lilypond]: http://lilypond.org/
[gnu-project]: http://gnu.org/

## Note Grouping

We need to define relations that constrain groups of notes depending on their
note values. To make it more practical we introduce a new level in our
hierarchy of sequences that represents note groups. A bar consist of a seqence
of groups of notes, rather than a sequence of notes. [Score 10](#score-10)
contains the note groupings that we will support.

{% lilypond Matched note groups in the groupo relation. %}
\relative {
  \override Staff.TimeSignature #'stencil = ##f
  \override Staff.Clef #'stencil = ##f
  \time 1/4
  c'16 c c c
  c16 c8 c16
  c8 c16 c16
  c16 c16 c8
  c8 c8
  \time 2/4
  c8 c4 c8
  \once \override Score.BarLine.break-visibility = ##(#f #t #t)
}
{% endlilypond %}

Let's define the new relation `groupo`. It is similar to our previous `baro`
relation, but matches the group of notes to ensure that the note values follow
one of the predefined patterns. The last branch matches on a group with a
single note longer or equal to a quarter note.

```clojure
(defn groupo [notes duration]
  (all
   (noteso notes)
   (note-valueo duration)
   (matche [notes]
           ([ [[_ 1] [_ 1] [_ 1] [_ 1]] ]
            (fd/== duration 4))
           ([ [[_ 1] [_ 2] [_ 1]] ]
            (fd/== duration 4))
           ([ [[_ 2] [_ 1] [_ 1]] ]
            (fd/== duration 4))
           ([ [[_ 1] [_ 1] [_ 2]] ]
            (fd/== duration 4))
           ([ [[_ 2] [_ 2]] ]
            (fd/== duration 4))
           ([ [[_ 2] [_ 4] [_ 2]] ]
            (fd/== duration 8))
           ([ [[_ v]] ]
            (fd/>= v 4)
            (fd/== duration v)))))
```


We also need the `groupso` relation for sequences of groups, with a parameter
for the total duration.

```clojure
(defne groupso [groups duration]
  ([ [] _ ]
   (fd/== duration 0))
  ([ [g . gs] _ ]
   (fresh [group-total sub-total]
     (groupo g group-total)
     (fd/+ group-total sub-total duration)
     (groupso gs sub-total))))
```

The `baro` relation can now be simplified to only constrain groups in a bar to
have a total duration of 16.

```clojure
(defn baro [groups]
  (groupso groups 16))
```

We need to flatten the groups to keep our external format intact.
This way note grouping is only a concern in the generator.

```clojure
(defn flatten-groups [groups]
  (map #(apply concat %1) groups))

(defn generate-score [n]
  (let [groups (run n [q]
                 (baro q))
        bars (flatten-groups groups)]
    {:bars (map ->bar bars)}))
```

To verify the behaviour of `groupo` we temporarily constrain the note pitch to
only take the value 1, the note C, by redefining `pitcho` in the REPL.

```clojure
smug.music> (defn pitcho [p]
              (fd/in p (fd/domain 1)))
#'smug.music/pitcho
```

As the first bars consist of simpler note values we generate a score of 64 bars
and drop the first half.

```clojure
smug.music> (clojure.pprint/pprint
             (->> (generate-score 64)
                  :bars
                  (drop 32)))
;; output:
(([:c 1/16] [:c 1/8] [:c 1/16] [:c 1/2] [:c 1/8] [:c 1/8])
 ([:c 1/4] [:c 1/8] [:c 1/16] [:c 1/16] [:c 1/2])
 ([:c 1/2] [:c 1/8] [:c 1/16] [:c 1/16] [:c 1/4])
 ([:c 1/4] [:c 1/8] [:c 1/8] [:c 1/4] [:c 1/8] [:c 1/8])
 ([:c 1/2] [:c 1/8] [:c 1/8] [:c 1/8] [:c 1/16] [:c 1/16])
 ([:c 1/8] [:c 1/8] [:c 1/4] [:c 1/4] [:c 1/8] [:c 1/8])
 ([:c 1/8] [:c 1/8] [:c 1/2] [:c 1/8] [:c 1/16] [:c 1/16])
 ([:c 1/4] [:c 1/4] [:c 1/4] [:c 1/8] [:c 1/16] [:c 1/16])
 ([:c 1/8] [:c 1/8] [:c 1/16] [:c 1/8] [:c 1/16] [:c 1/2])
 ([:c 1/4] [:c 1/4] [:c 1/16] [:c 1/8] [:c 1/16] [:c 1/4])
 ([:c 1/16] [:c 1/8] [:c 1/16] [:c 1/8] [:c 1/8] [:c 1/2])
 ([:c 1/8] [:c 1/16] [:c 1/16] [:c 1/2] [:c 1/4])
 ([:c 1/4] [:c 1/8] [:c 1/8] [:c 1/8] [:c 1/8] [:c 1/4])
 ([:c 1/8] [:c 1/8] [:c 1/4] [:c 1/8] [:c 1/8] [:c 1/4])
 ([:c 1/4] [:c 1/16] [:c 1/8] [:c 1/16] [:c 1/4] [:c 1/4])
 ([:c 1/8] [:c 1/8] [:c 1/8] [:c 1/8] [:c 1/4] [:c 1/4])
 ([:c 1/16] [:c 1/8] [:c 1/16] [:c 1/4] [:c 1/4] [:c 1/4])
 ([:c 1/2] [:c 1/16] [:c 1/8] [:c 1/16] [:c 1/16] [:c 1/8] [:c 1/16])
 ([:c 1/4] [:c 1/4] [:c 1/8] [:c 1/4] [:c 1/8])
 ([:c 1/4] [:c 1/4] [:c 1/8] [:c 1/8] [:c 1/16] [:c 1/8] [:c 1/16])
 ([:c 1/16] [:c 1/8] [:c 1/16] [:c 1/2] [:c 1/16] [:c 1/8] [:c 1/16])
 ([:c 1/2] [:c 1/8] [:c 1/8] [:c 1/16] [:c 1/16] [:c 1/8])
 ([:c 1/4] [:c 1/8] [:c 1/8] [:c 1/4] [:c 1/16] [:c 1/8] [:c 1/16])
 ([:c 1/8] [:c 1/8] [:c 1/2] [:c 1/16] [:c 1/16] [:c 1/8])
 ([:c 1/8] [:c 1/8] [:c 1/4] [:c 1/4] [:c 1/16] [:c 1/8] [:c 1/16])
 ([:c 1/4] [:c 1/4] [:c 1/4] [:c 1/16] [:c 1/16] [:c 1/8])
 ([:c 1/2] [:c 1/8] [:c 1/16] [:c 1/16] [:c 1/8] [:c 1/8])
 ([:c 1/8] [:c 1/4] [:c 1/8] [:c 1/2])
 ([:c 1/4] [:c 1/4] [:c 1/16] [:c 1/8] [:c 1/16] [:c 1/8] [:c 1/8])
 ([:c 1/4] [:c 1/16] [:c 1/16] [:c 1/8] [:c 1/2])
 ([:c 1/2] [:c 1/16] [:c 1/16] [:c 1/8] [:c 1/4])
 ([:c 1/8] [:c 1/16] [:c 1/16] [:c 1/2] [:c 1/8] [:c 1/8]))
```

Looks good. [Score 11](#score-11) shows the rendered result.

{% lilypond A generated score with note groups. %}
{
   c'16 c'8 c'16 c'2 c'8 c'8 \bar "|" c'4 c'8 c'16 c'16 c'2 \bar "|" c'2 c'8 c'16 c'16 c'4 \bar "|" c'4 c'8 c'8 c'4 c'8 c'8 \bar "|" c'2 c'8 c'8 c'8 c'16 c'16 \bar "|" c'8 c'8 c'4 c'4 c'8 c'8 \bar "|" c'8 c'8 c'2 c'8 c'16 c'16 \bar "|" c'4 c'4 c'4 c'8 c'16 c'16 \break 
   c'8 c'8 c'16 c'8 c'16 c'2 \bar "|" c'4 c'4 c'16 c'8 c'16 c'4 \bar "|" c'16 c'8 c'16 c'8 c'8 c'2 \bar "|" c'8 c'16 c'16 c'4 c'2 \bar "|" c'4 c'8 c'8 c'8 c'8 c'4 \bar "|" c'8 c'8 c'4 c'8 c'8 c'4 \bar "|" c'4 c'16 c'8 c'16 c'4 c'4 \bar "|" c'8 c'8 c'8 c'8 c'4 c'4 \break 
   c'16 c'8 c'16 c'4 c'4 c'4 \bar "|" c'2 c'16 c'8 c'16 c'16 c'8 c'16 \bar "|" c'4 c'4 c'8 c'4 c'8 \bar "|" c'4 c'4 c'8 c'8 c'16 c'8 c'16 \bar "|" c'16 c'8 c'16 c'2 c'16 c'8 c'16 \bar "|" c'2 c'8 c'8 c'16 c'16 c'8 \bar "|" c'4 c'8 c'8 c'4 c'16 c'8 c'16 \bar "|" c'8 c'8 c'2 c'16 c'16 c'8 \break 
   c'8 c'8 c'4 c'4 c'16 c'8 c'16 \bar "|" c'4 c'4 c'4 c'16 c'16 c'8 \bar "|" c'2 c'8 c'16 c'16 c'8 c'8 \bar "|" c'8 c'4 c'8 c'2 \bar "|" c'4 c'4 c'16 c'8 c'16 c'8 c'8 \bar "|" c'4 c'16 c'16 c'8 c'2 \bar "|" c'2 c'16 c'16 c'8 c'4 \bar "|" c'8 c'16 c'16 c'2 c'8 c'8 \bar "|."
}
{% endlilypond %}

Sweet, it works! That concludes our work on note groups, for now.

## Summary

We have gone through the basic music theory we need for our project, and
created a simple generator on which we can improve. There are some problems
with the relations still, like `(run 8 [q] (groupo q 16))` not terminating.
This seems to cause the same behaviour when trying to generate a score of more
than 308 bars. If anyone knows why, please let me know. I'll try to find out
why before the next post.

Anyway, we can now cross of the first three points on our list:

1. <s>Rythmic variation</s>
1. <s>Pitch variation</s>
1. <s>Common rythmic patterns</s>
1. Interesting pitch variations (melodies, scales, patterns)
1. Rests
1. Key signatures, modes, accidentals
1. Parameterized difficulty
1. Playback (generate a MIDI or WAV file)
1. Odd time signatures

We still have no randomness in our generator, so we get the same result every
time. This really defeats the purpose of a sight-reading exercise generator.
Even if we eventually need to add it, I think it's a good idea to wait until we
have nailed our other objectives. This way, it is much easier to verify how or
relations work.

The SMUG source code for this post is available on GitHub [at the `blog-post-1`
tag][github-code]. I hope you enjoyed this post and will continue following
the series. Don't forget to help me steer this thing in the right direction by
letting me know what's interesting to read about.

[github-code]: https://github.com/owickstrom/smug/tree/blog-post-1
