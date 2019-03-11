---
layout: post
title: "Property-Based Testing the Ugly Parts, Case Study 1: Timeline Flattening"
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

- Parts
    - Sequences
    - Parallels
    - Tracks
    - Clips and gaps
- Organizational
- Use to synchronize

### Timeline Flattening

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

# Hierarchical Timeline{background=#dddddd}

## Clips

![Clips](assets/property-based-testing-the-ugly-parts/timeline1.svg){width=80%}

<aside class="notes">
- Clips are put in video and audio tracks within parallels
- Tracks are played in parallel, hence the name
</aside>

## Video Still Frames

![Video Still Frames](assets/property-based-testing-the-ugly-parts/timeline2.svg){width=80%}

<aside class="notes">
If the video track is shorter, it will be padded with still frames
</aside>

## Adding Gaps

![Adding Gaps](assets/property-based-testing-the-ugly-parts/timeline3.svg){width=100%}

<aside class="notes">
- You can add explicit gaps in video and audio tracks
- These are also filled with still frames for video
</aside>

## Sequences

![Sequences](assets/property-based-testing-the-ugly-parts/timeline4.svg){width=100%}

<aside class="notes">
- Parallels are put in sequences
- Each parallel is played until its end, then the next, and so on
- Multiple parallels can be used to synchronize clips
</aside>

## Timeline

![Timeline](assets/property-based-testing-the-ugly-parts/timeline5.svg){width=100%}

<aside class="notes">
- The top level is the timeline
- The timeline contain sequences
- It's useful for organizing the parts of your screencast
</aside>

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
