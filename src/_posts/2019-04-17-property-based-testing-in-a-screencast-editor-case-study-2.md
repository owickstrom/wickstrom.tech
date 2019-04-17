---
layout: post
title: "Property-Based Testing in a Screencast Editor, Case Study 2: Video Scene Classification"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
excerpt: |
  In the last case study on property-based testing (PBT) in Komposition we
  looked at timeline flattening. This post covers the video classifier, how it
  was tested before, and the bugs I found when I wrote property tests for it.
---

In [the last case
study](/programming/2019/03/24/property-based-testing-in-a-screencast-editor-case-study-1.html)
on property-based testing (PBT) in Komposition we looked at timeline
flattening. This post covers the video classifier, how it was tested
before, and the bugs I found when I wrote property tests for it.

If you haven't read [the
introduction](/programming/2019/03/02/property-based-testing-in-a-screencast-editor-introduction.html)
or [the first case
study](/programming/2019/03/24/property-based-testing-in-a-screencast-editor-case-study-1.html)
yet, I recommend checking them out!

## Classifying Scenes in Imported Video

Komposition can automatically classify _scenes_ when importing video
files. This is a central productivity feature in the application,
effectively cutting recorded screencast material automatically,
letting the user focus on arranging the scenes of their
screencast. Scenes are segments that are considered _moving_, as
opposed to _still_ segments:

* A still segment is a sequence of at least $S$ seconds of
  _near-equal_ frames
* A moving segment is a sequence of _non-equal_ frames, or a sequence
  of near-equal frames with a duration less than $S$

$S$ is a preconfigured minimum still segment duration in
Komposition. In the future it might be configurable from the user
interface, but for now it's hard-coded.

Equality of two frames $f_1$ and $f_2$ is defined as a function
$E(f_1, f_2)$, described informally as:

* comparing corresponding pixel color values of $f_1$ and $f_2$, with
  a small epsilon for tolerance of color variation, and
* deciding two frames equal when at least 99% of corresponding pixel
  pairs are considered equal.

In addition to the rules stated above, there are two edge cases:

1. The first segment is always a considered a moving segment (even if
   it's just a single frame)
1. The last segment may be a still segment with a duration less than
   $S$
   
The second edge case is not what I would call a desirable feature, but
rather a shortcoming due to the classifier not doing any type of
backtracking. This could be changed in the future.

## Manually Testing the Classifier

The first version of the video classifier had no property
tests. Instead, I wrote what I thought was a decent classifier
algorithm, mostly messing around with various pixel buffer
representations and parallel processing to achieve acceptable
performance.

The only type of testing I had available, except for general use of
the application, was a color-tinting utility. This was a separate
program using the same classifier algorithm. It took as input a video
file, and produced as output a video file where each frame was tinted
green or red, for moving and still frames, respectively.

![Video classification shown with color tinting](/assets/property-based-testing-the-ugly-parts/color-tinting-cropped.gif){width=600 height=468}

In the recording above you see the color-tinted output video based on
a recent version of the classifier. It classifies moving and still
segments rather accurately. Before I wrote property tests and fixed
the bugs that I found, it did not look so pretty, flipping back and
forth at seemingly random places.


At first, debugging the classifier with the color-tinting tool way
seemed like a creative and powerful technique.  But the feedback loop
was horrible, having to record video, process it using the slow
color-tinting program, and inspecting it by eye.  In hindsight, I can
conclude that PBT is far more effective for testing the classifier.

## Video Classification Properties

Figuring out how to write property tests for video classification
wasn't obvious to me. It's not uncommon in example-based testing that
tests end up mirroring the structure, and even the full implementation
complexity, of the system under test. The same can happen in
property-based testing.

With some complex systems it's very hard to describe the correctness
as a relation between any valid input and the system's observed
output. The video classifier is one such case. How do I decide if an
output classification is correct for a specific input, without
reimplementing the classification itself in my tests?

The other way around is easy, though! If I have a classification, I
can convert that into video frames. Thus, the solution to the testing
problem is to not generate the input, but instead generate the
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
  flipping between all gray and all white pixels
* Still frames are converted to a sequence of frames containing all
  black pixels
  
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

## Testing Still Segment Minimum Length

As stated in the beginning of this post, classified still segments
must have a duration greater than or equal to $S$, where $S$ is the
minimum still segment duration used as a parameter for the classifier.
The first property test we'll look at asserts that this invariant
holds for all classification output.

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

This chunk of test code is pretty busy, and it's using a few helper
functions that I'm not going to bore you with. At a high level, this
test:

1. Generates a minimum still segment duration, based on a minimum
   frame count (let's call it $n$) in the range $[2, 20]$. The
   classifier currently requires that $n \geq 2$, hence the lower
   bound. The upper bound of 20 frames is an arbitrary number that
   I've chosen.
1. Generates valid output segments using the custom generator
   `genSegments`, where
     * moving segments have a frame count in $[1, 2n]$, and
     * still segments have a frame count in $[n, 2n]$.
1. Converts the generated output segments to actual pixel frames. This
   is done using a helper function that returns a list of alternating
   gray and white frames, or all black frames, as described earlier.
1. Count the number of consecutive frames within each segment, producing
   a list like `[Moving 18, Still 5, Moving 12, Still 30]`.
1. Performs a sanity check that the number of frames in the generated
   expected output is equal to the number of frames in the classified
   output. The classifier must not lose or duplicate frames.
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

## Sidetrack: Why generate the output?

Now, you might wonder why I generate output segments first, and then
convert to pixel frames. Why not generate random pixel frames to begin
with? The property test above only checks that the still segments are
long enough!

The benefit of generating valid output becomes clearer in the next
property test, where I use it as the expected output of the
classifier. Converting the output to a sequence of pixel frames is
easy, and I don't have to state any complex relation between the input
and output in my property. When using oracle generators, the
assertions can often be plain equality checks on generated and actual
output.

But there's benefit in using the same oracle generator for the
"minimum still segment length" property, even if it's more subtle. By
generating valid output and converting to pixel frames, I can generate
inputs that cover the edge cases of the system under test. Using
property test statistics and coverage checks, I could inspect
coverage, and even fail test runs where the generators don't hit
enough of the cases I'm interested in.[^2]

Had I generated random sequences of pixel frames, then perhaps the
majority of the generated examples would only produce moving segments.
I could tweak the generator to get closer to either moving or still
frames, within some distribution, but wouldn't that just be a
variation of generating valid scenes? It would be worse, in fact. I
wouldn't then be reusing existing generators, and I wouldn't have a
high-level representation that I could easily convert from and compare
with in assertions.

## Testing Moving Segment Time Spans

The second property states that the classified moving segments must
start and end at the same timestamps as the moving segments in the
generated output. Compared to the previous property, the relation
between generated output and actual classified output is stronger.

```{.haskell}
hprop_classifies_same_scenes_as_input = property $ do
  -- 1. Generate a minimum still still segment duration
  minStillSegmentFrames <- forAll $ Gen.int (Range.linear 2 (2 * frameRate))
  let minStillSegmentTime = frameCountDuration minStillSegmentFrames

  -- 2. Generate test segments
  segments <- forAll $ genSegments (Range.linear 1 10)
                                   (Range.linear 1
                                                 (minStillSegmentFrames * 2))
                                   (Range.linear minStillSegmentFrames
                                                 (minStillSegmentFrames * 2))
                                   resolution

  -- 3. Convert test segments to actual pixel frames
  let pixelFrames = testSegmentsToPixelFrames segments

  -- 4. Convert expected output segments to a list of expected time spans
  --    and the full duration
  let durations = map segmentWithDuration segments
      expectedSegments = movingSceneTimeSpans durations
      fullDuration = foldMap unwrapSegment durations

  -- 5. Classify movement of frames
  let classifiedFrames =
        Pipes.each pixelFrames
        & classifyMovement minStillSegmentTime
        & Pipes.toList

  -- 6. Classify moving scene time spans
  let classified =
        (Pipes.each classifiedFrames
         & classifyMovingScenes fullDuration)
        >-> Pipes.drain
        & Pipes.runEffect
        & runIdentity

  -- 7. Check classified time span equivalence
  expectedSegments === classified

  where
    resolution = 10 :. 10
```

Steps 1–3 are the same as in the previous property test. From there, this test:

4. Converts the generated output segments into a list of time
   spans. Each time span marks the start and end of an expected moving
   segment. Furthermore, it needs the full duration of the input in
   step 6, so that's computed here.
5. Classify the movement of each frame, i.e. if it's part of a moving
   or still segment.
6. Run the second classifier function called `classifyMovingScenes`,
   based on the full duration and the frames with classified movement
   data, resulting in a list of time spans.
7. Compare the expected and actual classified list of time spans.

While this test looks somewhat complicated with its setup and various
conversions, the core idea is simple. But is it effective?

### Bugs! Bugs everywhere!

Preparing for a talk on property-based testing, I added the "moving
segment time spans" property a week or so before the event. At this
time, I had used Komposition to edit multiple screencasts. Surely, all
significant bugs were caught already. Adding property tests should
only confirm the level of quality the application already had. Right?

Nope. First, I discovered that my existing tests were fundamentally
incorrect to begin with. They were not reflecting the specification I
had in mind, the one I described in the beginning of this post.

Furthermore, I found that the generators had errors. At first, I used
Hedgehog to generate the pixels used for the classifier input. Moving
frames were based on a majority of randomly colored pixels and a small
percentage of equally colored pixels. Still frames were based on a
random single color.

The problem I had not anticipated was that the colors used in moving
frames were not guaranteed to be distinct from the color used in still
frames. In small-sized examples I got black frames at the beginning
and end of moving segments, and black frames for still segments,
resulting in different classified output than expected. Hedgehog
shrinking the failing examples' colors towards 0, which is black,
highlighted this problem even more.

I made my generators much simpler, using the alternating white/gray
frames approach described earlier, and went on to running my new shiny
tests. Here's what I got:

![](/assets/property-based-testing-the-ugly-parts/video-classification-failure.png)

What? Where does 0s–0.6s come from? The classified time span should've
been 0s–1s, as the generated output has a single moving scene of 10
frames (1 second at 10 FPS). I started digging, using the `annotate`
function in Hedgehog to inspect the generated and intermediate values
in failing examples.

I couldn't find anything incorrect in the generated data, so I shifted
focus to the implementation code. The end timestamp 0.6s was
consistently showing up in failing examples. Looking at the code, I
found a curious hard-coded value 0.5 being bound and used locally in
`classifyMovement`.

The function is essentially a _fold_ over a stream of frames, where
the accumulator holds vectors of previously seen and
not-yet-classified frames. Stripping down and simplifying the old code
to highlight one of the bugs, it looked something like this:

```haskell
classifyMovement minStillSegmentTime =
  case ... of
    InStillState{..} ->
      if someDiff > minEqualTimeForStill
        then ...
        else ...
    InMovingState{..} ->
      if someOtherDiff >= minStillSegmentTime
        then ...
        else ...
  where
    minEqualTimeForStill = 0.5
```

Let's look at what's going on here. In the `InStillState` branch it
uses the value `minEqualTimeForStill`, instead of always using the
`minStillSegmentTime` argument. This is likely a residue from some
refactoring where I meant to make the value a parameter instead of
having it hard-coded in the definition.

Sparing you the gory implementation details, I'll outline two more
problems that I found. In addition to using the hard-coded value, it
incorrectly classified frames based on that value. Frames that
should've been classified as "moving" ended up "still". That's why I
didn't get 0s–1s in the output.

Why didn't I see 0s–0.5s, given the hard-coded value 0.5? Well, there
was also an off-by-one bug, in which one frame was classified
incorrectly together with the accumulated moving frames.

The `classifyMovement` function is 30 lines of Haskell code juggling
some state, and I managed to mess it up in three separate ways at the
same time. With these tests in place I quickly found the bugs and
fixed them. I ran thousands of tests, all passing.

Finally, I ran the application, imported a previously recorded video,
and edited a short screencast. The classified moving segments where
_notably_ better than before.

## Summary

A simple streaming fold can hide bugs that are hard to detect with
manual testing. The consistent result of 0.6, together with the
hard-coded value 0.5 and a frame rate of 10 FPS, pointed clearly
towards an off-by-one bug. I consider this is a great showcase of how
powerful shrinking in PBT is, consistently presenting minimal examples
that point towards specific problems. It's not just a party trick on
ideal mathematical functions.

Could these errors have been caught without PBT? I think so, but what
effort would it require? Manual testing and introspection did not work
for me. Code review might have revealed the incorrect definition of
`minEqualTimeForStill`, but perhaps not the off-by-one and incorrect
state handling bugs. There are of course many other QA techniques, I
won't evaluate all. But given the low effort that PBT requires in this
setting, the amount of problems it finds, and the accuracy it provides
when troubleshooting, I think it's a clear win.

I also want to highlight the iterative process that I find naturally
emerges when applying PBT:

1. Think about how your system is supposed to
   work. Write down your _specification_.
2. Think about how to generate input data and how to test your system,
   based on your specification. Tune your generators to provide better
   test data. Try out alternative styles of properties. Perhaps
   model-based or metamorphic testing fits your system better.
3. Run tests and analyze the minimal failing examples. Fix your
   implementation until all tests pass.

This can be done when modifying existing code, or when writing new
code. You can apply this without having any implementation code yet,
perhaps just a minimal stub, and the workflow is essentially the same
as TDD.

## Coming Up

The final post in this series will cover testing at a higher level of
the system, with effects and multiple subsystems being integrated to
form a full application. We will look at property tests that found
many bugs and that made a substantial refactoring possible.

1. [Introduction](/programming/2019/03/02/property-based-testing-in-a-screencast-editor-introduction.html)
1. [Timeline Flattening](/programming/2019/03/24/property-based-testing-in-a-screencast-editor-case-study-1.html)
1. **Video Scene Classification**
1. Integration Testing (up next!)

Until then, thanks for reading!

## Credits

Thank you Ulrik Sandberg for reviewing drafts of this post.

## Footnotes

[^1]: See the "Oracle Generators" section in [Finding Property Tests](https://www.hillelwayne.com/post/contract-examples/).
[^2]: John Hughes' talk [Building on developers' intuitions](https://www.youtube.com/watch?v=NcJOiQlzlXQ) goes into depth on this. There's also [work being done](https://github.com/hedgehogqa/haskell-hedgehog/pull/253) to provide similar functionality for Hedgehog.
