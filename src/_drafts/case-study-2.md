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
we looked at timeline flattening. This one is not quite as long, I
promise! It covers the video classifier, how it was tested before, and
the bugs I found when I wrote property-based tests for it.

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

Equality of frames is defined as a function $E(f_1, f_2)$, described
informally as:

* comparing corresponding pixel color values of $f_1$ and $f_2$, with
  a small epsilon tolerance, and
* deciding two frames equal when at least 99% of corresponding pixel
  pairs are considered equal.

In addition to the rules stated above, there are two edge cases:

1. The first segment is always a considered a moving segment (even if
   it's just a single frame!)
1. The last segment may be a still segment with a duration less than
   $S$
   
The second edge case is not what I would call a desirable feature, but
rather a shortcoming due to the classifier not doing any type of
backtracking. This could be changed in the future.

## Manually Testing the Classifier

The first version of the video classifier had no property
tests. Instead, I wrote what I thought was a decent classifier
algorithm, mostly messing around with various pixel buffer
representations and parallel processing to get decent performance.

The only type of testing I had available, except for general use of
the application, was a color-tinting utility. This was a separate
program using the same classifier algorithm. It took as input a video
file, and produced as output a video file where each frame was tinted
green or red, for moving and still frames, respectively.

![Video classification shown with color tinting](/assets/property-based-testing-the-ugly-parts/color-tinting-cropped.gif){width=600 height=468}

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

## Video Classification Properties

Writing properties for video classification turned out harder than an
initially thought it would be. It's not uncommon in example-based
testing that tests end up mirroring the structure, and even the full
implementation complexity, of the system under test. This can happen
in property-based testing, too.

With some complex systems it's very hard to describe the correctness
as a relation between any valid input and the system's observed
output. The video classifier is one such case. How do I decide if an
output classification is correct for any valid input, without
reimplementing the classification itself in my tests?

The other way around is easy, though! If I have a classification, I
can easily convert that into video frames. Thus, the solution to the
testing problem is to not generate the input, but instead generate the
_expected output_. Hillel Wayne calls this technique "oracle
generators" in his recent article.[^1]

The classifier property tests generate high-level representations of
the expected classification output, which are lists of values
describing the type and duration of segments.
 
{% diagram :width => 600, :caption => "A generated sequence of expected classified segments" %}
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

Next, the list of output segments is converted into a sequence of
actual frames. Frames are two-dimensional arrays of RGB pixel
values. The conversion is simple: 

* Moving segments are converted to a sequence of alternating frames,
  flipping between all grey and all white pixels
* Still frames converted to a sequence of frames containing all black
  pixels
  
The example sequence in the diagram above, when converted to pixel
frames with a frame rate of 10 FPS, can be visualized like in the
following diagram, where each thin rectangle represents a frame:

{% diagram :width => 600, :caption => "Pixel frames derived from a sequence of expected classified output segments" %}
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

By generating high-level output and converting it to pixel frames, I
have input to feed the classifier with, and I know what output it
should produce. Writing effective property tests then comes down to
writing generators that produce valid output, according to the
specification of the classifier. In this post I'll show two such
property tests.

### Testing Still Segment Minimum Length

As stated in the beginning of this post, classified still segments
must have a duration greater than or equal to $S$, where $S$ is the
mininum still segment duration used as a parameter for the classifier.
The first property test we'll look at asserts that this invariant
holds for all classification outputs.

```{.haskell}
hprop_classifies_still_segments_of_min_length = property $ do

  -- 1. Generate a minimum still segment length/duration
  minStillSegmentFrames <- forAll $ Gen.int (Range.linear 2 (2 * frameRate))
  let minStillSegmentTime = frameCountDuration minStillSegmentFrames

  -- 2. Generate output segments
  segments <- forAll $
    genSegments (Range.linear 1 10)
                (Range.linear 1
                              (minStillSegmentFrames * 2))
                (Range.linear minStillSegmentFrames
                              (minStillSegmentFrames * 2))
                resolution

  -- 3. Convert test segments to actual pixel frames
  let pixelFrames = testSegmentsToPixelFrames segments

  -- 4. Run the classifier on the pixel frames
  let counted = classifyMovement minStillSegmentTime (Pipes.each pixelFrames)
                & Pipes.toList
                & countSegments

  -- 5. Sanity check
  countTestSegmentFrames segments === totalClassifiedFrames counted

  -- 6. Ignore last segment and verify all other segments
  case initMay counted of
    Just rest ->
      traverse_ (assertStillLengthAtLeast minStillSegmentTime) rest
    Nothing -> success
  where
    resolution = 10 :. 10
```

There's a lot going on in this code, and it's using a few helper
functions that I'm not going to bore you with. At a high level, this
test:

1. Generates a minimum still segment duration, based on a
   minimum frame count (let's call it $n$).
1. Generates valid output segments using the custom generator
   `genSegments`, where
     * moving segments have a frame count in $[1, 2n]$, and
     * still segments have a frame count in $[n, 2n]$.
1. Converts the expected output segments to actual pixel frames. This
   is done using a helper function that returns a list of alternating
   gray and white frames, or all black frames, as described earlier.
1. Count the number of consecutive frames within each segment, producing
   a list like `[Moving 18, Still 5, Moving 12, Still 30]`.
1. Performs a sanity check that the number of frames in the generated
   expected output as equal to the number of frames in the classified
   output.
1. Drops the last classified segment, which according to the
   specification can have a frame count less than $n$, and asserts
   that all other still segments have a frame count greater than or
   equal to $n$.
   
Let's run some tests.

```{.text}
> :{
| hprop_classifies_still_segments_of_min_length
|   & Hedgehog.withTests 10000
|   & Hedgehog.check
| :}
  ✓ <interactive> passed 10000 tests.
```

Cool, it looks like it's working.

### Sidetrack: Why generate the output?

Now, you might wonder why I need to generate output segments first,
and then convert to pixel frames? Why not generate random pixel frames
to begin with? The test only checks that the still segments are long
enough without really caring about the generated outputs.

The benefit of generating valid outputs is much more clear in the next
property test, but there's benefit for these tests too, although more
subtle. By generating valid outputs and converting to pixel frames, we
can generate inputs that cover the edge cases of our system under
test. Using property test statistics and coverage checks, we can even
fail test runs where the generators don't hit enough of the cases we're
interested in.[^2]

Had I generated random sequences of pixel frames, then perhaps the
majority of the generated examples would only produce moving segments.
I could tweak the generator to get closer to either moving or still
frames, within some distribution, but wouldn't that just be a
variation of generating valid scenes? It would be worse, as I wouldn't
then be reusing existing generators, and I wouldn't have a high-level
representation that I could easily convert from and compare with in
assertions.

### Testing Moving Segment Timespans

The second property tests that the moving 

2. Classified moving segments must have correct timespans
   - Comparing the generated _expected_ output to the classified
     timespans
   - (here were bugs!)


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

(REST IS TODO...)

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

[^1]: See the "Oracle Generators" section in [Finding Property Tests](https://www.hillelwayne.com/post/contract-examples/).
[^2]: John Hughes' talk [Building on developers' intuitions](https://www.youtube.com/watch?v=NcJOiQlzlXQ) goes into depth on this. There's also [work being done](https://github.com/hedgehogqa/haskell-hedgehog/pull/253) to provide similar functionality for Hedgehog.
