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

![Clips and gaps are placed in video and audio tracks](assets/property-based-testing-the-ugly-parts/timeline1.svg){width=80%}

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

![Still frames are automatically inserted to match track durations](assets/property-based-testing-the-ugly-parts/timeline2.svg){width=80%}

You can also add gaps manually, specifying a duration of the gap and
inserting it into a video or audio track. The following diagram shows
a parallel with manually added gaps in both video and audio
tracks.

![Adding Gaps](assets/property-based-testing-the-ugly-parts/timeline3.svg){width=100%}

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

- Motivation
  - Komposition uses FFmpeg to render the final video file
  - FFmpeg does not know about timeline hierarchy, only flat video and
    audio tracks
  - Flattening converts the hierarchical timeline to a flat timeline:
    - Video track
    - Audio track

## Property Tests

- Duration
- Clip occurence
- Still frames used in gaps
- Flattening equivalences

### Missing properties
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

# Case Study 1: Timeline Flattening
  
## Timeline Flattening (Graphical)

![Timeline flattening](assets/property-based-testing-the-ugly-parts/komposition-flattening.svg){width=100%}

## Testing Duration

```{.haskell emphasize=5:5-5:99}
hprop_flat_timeline_has_same_duration_as_hierarchical = property $ do
  timeline' <- forAll $
    Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
  let Just flat = Render.flattenTimeline timeline'
  durationOf AdjustedDuration timeline' === durationOf AdjustedDuration flat
````

## Testing Clip Occurence

```{.haskell emphasize=5:5-5:99,6:5-6:99}
hprop_flat_timeline_has_same_clips_as_hierarchical = property $ do
  -- Generate a timeline with clips in each parallel
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
