---
layout: post
title: "Property-Based Testing in a Screencast Editor, Case Study 2: Video Scene Classification"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
excerpt: |
  TODO
---

In [the last case
study](/programming/2019/03/24/property-based-testing-in-a-screencast-editor-case-study-1.html)
we looked at timeline flattening. This one is not as long, I promise!
It covers the video classifier, how it was tested before, and the bugs
I found when I wrote property-based tests for it.

## Classifying Scenes in Imported Video

Komposition can automatically classify _scenes_ when importing video
files. Scenes are segments that are considered either _still_ or _moving_:

* A still segment is a sequence of at least $S$ seconds of
  _near-equal_ frames
* A moving segment is a sequence of _non-equal_ frames, or a sequence
  of near-equal frames with a duration less than $S$

$S$ is a preconfigured threshold for still segment duration. In the
future it might be configurable from the user interface, but for now
it's hardcoded in the application.

In addition to the rules stated above, there are two edge cases:

1. The first segment is always a considered a moving segment (even if
   it's just a single frame!)
1. The last segment may be a still segment with a duration less than
   $S$
   
The second edge case is not what I would call a desirable feature, but
rather a shortcoming due to the classifier not doing any type of
backtracking. This could be changed in the future.

## Testing Video Classification

The first version of the video classifier had no property
tests. Instead, I wrote what I thought was a decent classifier
algorithm, mostly messing around with various pixel buffer
representations and parallel processing to get decent performance.

The only type of testing I had available, except for general use of
the application, was a color-tinting utility. This was a separate
program using the same classifier algorithm. It took as input a video
file, and produced as output a video file where each frame was tinted
green or red, for moving and still frames, respectively.

![Video classification shown with color tinting](/assets/property-based-testing-the-ugly-parts/color-tinting.gif)

In the recording above you see the color-tinted output video based on
a recent version of the classifier. It classifies moving and still
segments rather accurately.

Before I wrote property tests and fixed the bugs that I found, it did
not look so pretty, flipping back and forth at seemingly random
places. Moreover, the feedback loop was horrible, having to record
video, process it using the slow color-tinting program, and inspecting
it by eye.

At first, debugging the classifier with the color-tinting tool way
seemed like a creative and powerful technique. In hindsight, I can
conclude that property-based testing is more effective for testing the
classifier.

### Video Classification Properties

Writing properties for video classification turned out harder than an
initially thought it would be.

I've used a techniques that Hillel Wayne calls _oracle generators_ in
his recent article.[^1] Instead of generating an input for the system
under test, and describing a relation between the input and observed
output, these properties generate the _expected output_.

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

[^1]: See the "Oracle Generators" section in [Finding Property Tests](https://www.hillelwayne.com/post/contract-examples/)
