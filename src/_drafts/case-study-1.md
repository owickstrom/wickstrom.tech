---
layout: post
title: "Property-Based Testing in a Screencast Editor, Case Study 1: Timeline Flattening"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
draft: true
excerpt: |
---

In the first post of this series I introduced the Komposition
screencast editor, and briefly explained the fundamentals of
property-based testing (PBT). Furthermore, I covered how to write
testable code, regardless of _how_ you check your code with automated
tests. Lastly, I highlighted some difficulties in using properties to
perform component and integration testing.

This post is the first case study in the series, covering the
_timeline flattening_ process in Komposition and how its tested using
PBT. The property tests aren't integration level tests, but rather
unit tests. This case study serves as a warm-up to the coming, more
advanced, ones.

Before we look at the tests, we need to learn more about Komposition's
hierarchical timeline and how the flattening process works.

## The Hierarchical Timeline

Komposition's timeline is hierarchical. While many non-linear editing
systems have support for some form of nesting (Final Cut Pro has
[compound clips](https://support.apple.com/kb/PH12631?locale=en_US),
and Adobe Premiere Pro has [nested
sequences](https://www.premiumbeat.com/blog/nesting-in-adobe-premiere-pro/))
they are primarily focused on flat timeline workflows. The timeline
structure and the keyboard-driven editing in Komposition is optimized
for the screencast editing workflow I use.

It's worth emphasizing that Komposition is _not_ a general video
editor. In addition to its specific editing workflow, you may need to
adjust your recording workflow to use it effectively. See the
[documentation on
workflow](https://owickstrom.github.io/komposition/user-guide/workflow/)
for more details.

### Video and Audio in Parallels

At the lowest level of the timeline are _clips_ and _gaps_. Those are
put within the video and audio _tracks_ of _parallels_. The following diagram
shows a parallel consisting of two video clips and one audio clip. 

![Clips and gaps are placed in video and audio tracks](assets/property-based-testing-the-ugly-parts/timeline1.svg){width=50%}

The tracks of a parallel are played simultaneously (in parallel), as
indicated by the arrows in the above diagram. The tracks start playing
at the same time. This makes parallels useful to synchronize the
playback of specific parts of a screencast, and to group closely
related clips.

### Gaps

When editing screencasts made up of separate video and audio recording
you often end up with differing clip durations. The voice-over audio
clip might be longer than the corresponding video clip, or vice versa.
A useful default behavior is to extend the short clips. For audio,
this is easy. Just pad with silence. For video, it's not so clear what
to do. In Komposition, shorter video tracks are padded with repeated
still frame sections called _gaps_.

The following diagram shows a parallel with a short video clips and a
longer audio clip. The dashed area represents the automatically
inserted gap.

![Still frames are automatically inserted to match track durations](assets/property-based-testing-the-ugly-parts/timeline2.svg){width=50%}

You can also add gaps manually, specifying a duration of the gap and
inserting it into a video or audio track. The following diagram shows
a parallel with manually added gaps in both video and audio
tracks.

![Adding Gaps](assets/property-based-testing-the-ugly-parts/timeline3.svg){width=50%}

Manually added gaps are padded with still frames or silence,
just as gaps added automatically to match track durations.

### Sequences

Parallels are put in _sequences_. The parallels within a sequence are
played sequentially; the first one is played in its entirety, then the
next one, and so on. This behavior is different from how parallels
play their tracks. Parallels and sequences, with their different
playback behaviors, make up the fundamental building blocks of the
compositional editing in Komposition.

The following diagram shows a sequence of two parallels, playing
sequentially:

![A sequence containing two parallels](assets/property-based-testing-the-ugly-parts/timeline4.svg){width=100%}

### The Timeline

Finally, at the top level, we have the _timeline_. Effectively, the
timeline is a sequence of sequences; it plays every child sequence in
sequence. The reason for this level to exist is for the ability to
group larger chunks of a screencast within separate sequences.

![Timeline](assets/property-based-testing-the-ugly-parts/timeline5.svg){width=100%}

I use separate sequences within the timeline to delimit distinct parts
of a screencast, such as the introduction, the different chapters, and
the summary.

## Timeline Flattening

Komposition currently uses [FFmpeg](https://ffmpeg.org/) to render the
final media. This is done by constructing an `ffmpeg` command
invocation with a [filter
graph](https://ffmpeg.org/ffmpeg-filters.htmL) describing how to fit
together all clips, still frames, and silent audio parts.

FFmpeg doesn't know about hierarchical timelines; it only cares about
video and audio streams. To convert the hierarchical timeline into a
suitable representation to build the FFmpeg filter graph from,
Komposition performs _timeline flattening_.

The flat representation of a timeline contains only two tracks; audio
and video. All gaps are _explicitly_ represented in those tracks. The
following graphs shows how a hierarhical timeline is flattened into
two tracks.

![Timeline flattening](assets/property-based-testing-the-ugly-parts/komposition-flattening.svg){width=100%}

Notice in the graphic above how the implicit gaps at the ends of video
and audio tracks get represented with explicit gaps in the flat
timeline. This is because FFmpeg does not know how to render implicit
gaps. All gaps are represented explicitly, and are converted to clips
of still frames or silent audio when rendered with FFmpeg.

## Property Tests

To test the timeline flattening, there are a number of properties that
are checked. I'll go through each one and their property test code.

### Duration Equality

Given a timeline $t$, where all parallels have at least one video clip,
the total duration of the flattened $t$ must be equal to the
total duration of $t$. Or, in a more dense notation,

$$\forall t \in T \to duration(flatten(t)) = duration(t)$$

where $T$ is the set of timelines with at least one video clip in each
parallel.

The reason that all parallels must have at least one video clip is
because currently the flattening algorithm can only locate still
frames for video gaps from within the same parallel. If it encounters
a parallel with no video clips, the timeline flattening fails. This
limitation is discussed in greater detail at the end of this article.

The test for the duration equality property is written using Hedgehog,
and looks like this:

```{.haskell emphasize=5:5-5:99}
hprop_flat_timeline_has_same_duration_as_hierarchical = property $ do
  -- Generate a timeline with video clips in each parallel
  timeline' <- forAll $
      Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips

  -- Flatten the timeline and extract the result
  let Just flat = Render.flattenTimeline timeline'
  
  -- Check that hierarchical and flat timeline durations are equal
  durationOf AdjustedDuration timeline' === durationOf AdjustedDuration flat
```

It generates a timeline using `forAll` and custom generators. Instead
of generating timelines of _any_ shape and filtering out only the ones
with video clips in each parallel, which would be very inefficient,
these tests use custom generators to only obtain inputs that satisfy
the invariants of the system.

The range passed as the first argument to `Gen.timeline` is used as
the bounds of the generator, such that each level in the generated
hierarhical timeline will have at most 5 children.

`Gen.timeline` takes as its second argument _another generator_, the
one used to generate parallels, which in this case is
`Gen.parallelWithClips`.  With Hedgehog generators being regular
values, it's practical to compose them like this. A "higher-order
generator" can be a regular function taking other generators as
arguments.

As you might have noticed, `durationOf` takes as its first argument a
value `AdjustedDuration`. What's that about? Well, Komposition
supports adjusting the playback speed of video media for individual
clips. To calculate the final duration of a clip, the playback speed
needs to taken into account. By passing `AdjustedDuration` we take
playback speed into account for all video clips.

Let's say I had introduced a bug in timeline flattening, in which all
video gaps weren't added correctly to the flat video tracks. The
flattening is implemented as a fold, and it would not be unthinkable
that the accumulator was incorrectly constructed in a case. The test
would catch this quickly and present us with a minimal
counter-example:

![Hedgehog presenting a minimal counter-example](assets/property-based-testing-the-ugly-parts/timeline-duration-failure.png){width=100%}

Hedgehog prints the source code for the failing property. Below the
`forAll` line the generated value is printed. The difference between
the expected and actual value is printed below the failing
assertion. In this case it's a simple expression of type
`Duration`. In case you're comparing large tree-like structures, this
diff will highlight only the differing expressions. Finally, it prints
the following:

```
This failure can be reproduced by running:
> recheck (Size 23) (Seed 16495576598183007788 5619008431246301857) <property>
```

When working on finding and fixing the fold bug, we can use the
printed _size_ and _seed_ values to deterministically run the exact
same test case over and over.

### Clip Occurence

Slightly more complicated, the next property checks that all clips
from the hierarchical timeline, and no other clips, occur within the
flat timeline. As discussed in the introduction on timeline
flattening, implicit gaps get converted to explicit gaps and thereby
adding more gaps, but no video or audio clips should be added or
removed.

```{.haskell emphasize=5:5-5:99,6:5-6:99}
hprop_flat_timeline_has_same_clips_as_hierarchical = property $ do
  -- Generate a timeline with video clips in each parallel
  timeline' <- forAll $
      Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
  
  -- Flatten the timeline
  let flat = Render.flattenTimeline timeline'
  
  -- Check that all video clips occur in the flat timeline
  flat ^.. _Just . Render.videoParts . each . Render._VideoClipPart
      === timelineVideoClips timeline'
  
  -- Check that all audio clips occur in the flat timeline
  flat ^.. _Just . Render.audioParts . each . Render._AudioClipPart
      === timelineAudioClips timeline'
```

The hierarchical timeline is generated and flattened like before. The
two assertions check that the respective video clips and audio clips
are equal. It's using lenses to extract clips from the flat timeline,
and the helper functions `timelineVideoClips` and `timelineAudioClips`
to extract clips from the original hierarhical timeline.

### Still Frames Used

### Flattening Equivalences

## Missing Properties

- Same flat result regardless of grouping
  - split/join sequences, then flatten

## A Missing Feature

- Padding with frames from other parallels
- Frames are only picked from video clips within the parallel
- Should pick from _any_ video clip within the timeline
- Write properties to guide my work

## Summary

  - We've looked at timeline flattening, the simplest case study in this series
  - Not an integration test or stubbing out complicated effectful computation
  - A good warmup
  - Next up is testing the video classifier
    - Also a pure function
    - More complicated and harder to test logic
