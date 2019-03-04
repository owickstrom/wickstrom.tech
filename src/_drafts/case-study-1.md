---
layout: post
title: "Property-Based Testing the Ugly Parts, Case Study 1: Timeline Flattening"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
draft: true
excerpt: |
---

* Intro
  - Last part introduced:
    - Komposition
    - PBT
    - Writing testable code
    - How PBT can be hard to in integration testing
  - This post:
    - First case study "Timeline flattening"
    - Warm-up
    - A collection of pure functions
* Hierarchical timeline
  - The timeline is hierarchical
    - Most NLEs have nested sequences support, but are centered around
      flat tracks
    - Parts
        - Sequences
        - Parallels
        - Tracks
        - Clips and gaps
    - Organizational
    - Use to synchronize
* Timeline Flattening
  - Motivation
    - Komposition uses FFmpeg to render the final video file
    - FFmpeg does not know about timeline hierarchy, only flat video and
      audio tracks
    - Flattening converts the hierarchical timeline to a flat timeline:
      - Video track
      - Audio track
  - Tests
    - Duration
    - Clip Occurence
  - Missing properties
    - How are video gaps padded with still frames?
      - TODO: How could this be tested?
    - Same flat result regardless of grouping
      - split/join sequences, then flatten
  - Missing feature
    - Padding with frames from other parallels
    - Frames are only picked from video clips within the parallel
    - Should pick from _any_ video clip within the timeline
    - Write properties to guide my work
- Wrapping up
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
hprop_flat_timeline_has_same_duration_as_hierarchical =
  property $ do
    t <- forAll $ Gen.timeline (Range.exponential 0 20) Gen.parallelWithClips
    let Just flat = Render.flattenTimeline t
    durationOf AdjustedDuration t === durationOf AdjustedDuration flat
````

## Testing Clip Occurence

```{.haskell emphasize=5:5-5:99,6:5-6:99}
hprop_flat_timeline_has_same_clips_as_hierarchical =
  property $ do
    t <- forAll $ Gen.timeline (Range.exponential 0 20) Gen.parallelWithClips
    let Just flat = Render.flattenTimeline t
    timelineVideoClips t === flatVideoClips flat
    timelineAudioClips t === flatAudioClips flat
```

## Further Improvements

* Missing properties
  - How are video gaps padded with still frames?
  - Same flat result regardless of grouping (split/join sequences, then flatten)
* Padding with frames from other parallels
  - Frames are only picked from video clips within the parallel
  - Should pick from _any_ video clip within the timeline
  - Write properties to guide my work

