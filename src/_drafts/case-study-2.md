---
layout: post
title: "Property-Based Testing in a Screencast Editor, Case Study 2: Video Scene Classification"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
excerpt: |
  TODO
---


## Video Scene Classification

* Komposition can automatically classify "scenes"
  * **Moving segment:** consecutive non-equal frames
  * **Still segment:** at least _S_ seconds of consecutive near-equal
    frames
* _S_ is a preconfigured threshold for still segment duration
* Edge cases:
  - First segment is always a moving segment
  - Last segment may be shorter

## Visualizing with Color Tinting

![Video classification shown with color tinting](/assets/property-based-testing-the-ugly-parts/color-tinting.gif)

## Testing Video Classification

* Generate high-level representation of _expected_ output segments
 
{% diagram :width => 600, :caption => "Clips and gaps are placed in video and audio tracks" %}
import           Diagrams.Direction (dir)
import           TimelineDiagrams
import           VideoClassifierDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  renderSegments (Id [1]) segments
  where
    segments = [ Segment Moving 4 (Just "Moving (4s)")
               , Segment Still 3 (Just "Still (3s)")
               , Segment Moving 8 (Just "Moving (8s)")
               ]
{% enddiagram %}

* Convert output representation to actual pixel frames
  - Moving frames: flipping between grey and white pixels
  - Still frames: all black pixels

{% diagram :width => 600, :caption => "Clips and gaps are placed in video and audio tracks" %}
import           Diagrams.Direction (dir)
import           TimelineDiagrams
import           VideoClassifierDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  renderPixelFrames (Id [2]) segments
  where
    segments = [ Segment Moving 4 (Just "Moving (4s)")
               , Segment Still 3 (Just "Still (3s)")
               , Segment Moving 8 (Just "Moving (8s)")
               ]
{% enddiagram %}

* Run the classifier on the pixel frames
* Test properties based on:
  - the expected output representation
  - the actual classified output

## Two Properties of Video Classification

1. Classified still segments must be at least _S_ seconds long
   - Ignoring the last segment (which may be a shorter still segment)
2. Classified moving segments must have correct timespans
   - Comparing the generated _expected_ output to the classified
     timespans
   - (here were bugs!)

## Testing Still Segment Lengths

```{.haskell}
hprop_classifies_still_segments_of_min_length = property $ do

  -- Generate test segments
  segments <- forAll $
    genSegments (Range.linear 1 (frameRate * 2)) resolution

  -- Convert test segments to actual pixel frames
  let pixelFrames = testSegmentsToPixelFrames segments

  ...
```

## Testing Still Segment Lengths (cont.)

```{.haskell}
  ...

  -- Run classifier on pixel frames
  let counted = classifyMovement 1.0 (Pipes.each pixelFrames)
                & Pipes.toList
                & countSegments

  -- Sanity check: same number of frames
  countTestSegmentFrames segments === totalClassifiedFrames counted

  -- Then ignore last segment (which can be a shorter still segment),
  -- and verify all other segments
  case initMay counted of
    Just rest -> traverse_ (assertStillLengthAtLeast 1.0) rest
    Nothing     -> success
  where
    resolution = 10 :. 10
```

## Success!

```{.text}
> Hedgehog.check hprop_classifies_still_segments_of_min_length
  ✓ <interactive> passed 100 tests.
```

## Testing Moving Segment Timespans

```{.haskell}
hprop_classifies_same_scenes_as_input = property $ do

  -- Generate test segments
  segments <- forAll
    genSegments (Range.linear (frameRate * 1) (frameRate * 5)) resolution

  -- Convert test segments to timespanned ones, and actual pixel frames
  let segmentsWithTimespans = segments
                              & map segmentWithDuration
                              & segmentTimeSpans
      pixelFrames = testSegmentsToPixelFrames segments
      fullDuration = foldMap
                     (durationOf AdjustedDuration . unwrapSegment)
                     segmentsWithTimespans

  ...
```

## Testing Moving Segment Timespans (cont.)

```{.haskell}
  ...

  -- Run classifier on pixel frames
  classified <-
    (Pipes.each pixelFrames
     & classifyMovement 1.0
     & classifyMovingScenes fullDuration)
    >-> Pipes.drain
    & Pipes.runEffect

  -- Check classified timespan equivalence
  unwrapScenes segmentsWithTimespans === classified

  where resolution = 10 :. 10
```

## Failure!

![](/assets/property-based-testing-the-ugly-parts/video-classification-failure.png)

## What Went Wrong?

* There were multiple bugs:
    - The specificiation was wrong
    - The generators and tests had errors
    - The implementation had errors (since its inception)
* Process:
  - Think about the specification first
  - Think about how generators and tests should work, rewrite them
  - Get minimal examples of failures, fix the implementation
* Thousands of tests ran successfully
* Tried importing actual recorded video, had great results!


## Credits

TODO...

## Footnotes

[^1]: ...
