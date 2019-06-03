---
layout: post
title: "Property-Based Testing in a Screencast Editor, Case Study 1: Timeline Flattening"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
excerpt: |
  This post is the first case study in the "Property-Based Testing in a
  Screencast Editor" series, covering the timeline flattening process in
  Komposition and how it's tested using PBT.
---

In [the first post of this
series](https://wickstrom.tech/programming/2019/03/02/property-based-testing-in-a-screencast-editor-introduction.html)
I introduced the Komposition screencast editor, and briefly explained
the fundamentals of property-based testing (PBT). Furthermore, I
covered how to write testable code, regardless of _how_ you check your
code with automated tests. Lastly, I highlighted some difficulties in
using properties to perform component and integration testing.

If you haven't read the introductory post, I suggest doing so before
continuing with this one. You'll need an understanding of what PBT is
for this case study to make sense.

This post is the first case study in the series, covering the
_timeline flattening_ process in Komposition and how it's tested using
PBT. The property tests aren't integration-level tests, but rather
unit tests. This case study serves as a warm-up to the coming, more
advanced, ones.

Before we look at the tests, we need to learn more about Komposition's
hierarchical timeline and how the flattening process works.

## The Hierarchical Timeline

Komposition's timeline is hierarchical. While many non-linear editing
systems have support for some form of nesting[^1] they are primarily
focused on flat timeline workflows. The timeline structure and the
keyboard-driven editing in Komposition is optimized for the screencast
editing workflow I use.

It's worth emphasizing that Komposition is not a general video
editor. In addition to its specific editing workflow, you may need to
adjust your recording workflow to use it effectively[^2].

### Video and Audio in Parallels

At the lowest level of the timeline are _clips_ and _gaps_. Those are
put within the video and audio _tracks_ of _parallels_. The following
diagram shows a parallel consisting of two video clips and one audio
clip.

{% diagram :width => 350, :caption => "Clips and gaps are placed in video and audio tracks" %}
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Parallel
    (Track Video [Clip 4 (Just "Video Clip 1"), Clip 5 (Just "Video Clip 2")])
    (Track Audio [Clip 6.5 (Just "Audio Clip 1")])
  # renderParallel defaultRenderSettings { parallelArrows = True } (Id [])
{% enddiagram %}

The tracks of a parallel are played simultaneously (in parallel), as
indicated by the arrows in the above diagram. The tracks start playing
at the same time. This makes parallels useful to synchronize the
playback of specific parts of a screencast, and to group closely
related clips.

### Gaps

When editing screencasts made up of separate video and audio
recordings you often end up with differing clip duration. The
voice-over audio clip might be longer than the corresponding video
clip, or vice versa.  A useful default behaviour is to extend the
short clips. For audio, this is easy. Just pad with silence. For
video, it's not so clear what to do. In Komposition, shorter video
tracks are padded with repeated still frame sections called _gaps_.

The following diagram shows a parallel with a short video clip and a
longer audio clip. The dashed area represents the implicit gap.

{% diagram :width => 350, :caption => "Still frames are automatically inserted at implicit gaps to match track duration" %}
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Parallel
    (Track Video [Clip 4 (Just "Video Clip 1"), Gap Implicit 4 (Just "Implicit Gap")])
    (Track Audio [Clip 8 (Just "Audio Clip 1")])
  # renderParallel defaultRenderSettings { parallelArrows = True } (Id [])
{% enddiagram %}

You can also add gaps manually, specifying a duration of the gap and
inserting it into a video or audio track. The following diagram shows
a parallel with manually added gaps in both video and audio
tracks.

{% diagram :width => 350, :caption => "Adding explicit gaps manually" %}
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Parallel
    (Track Video [Clip 4 (Just "Video Clip 1"), Gap Explicit 2 (Just "Gap"), Clip 3 (Just "Video Clip 2")])
    (Track Audio [Gap Explicit 2 (Just "Gap"), Clip 7 (Just "Audio Clip 1")])
  # renderParallel defaultRenderSettings { parallelArrows = True } (Id [])
{% enddiagram %}


Manually added gaps (called _explicit_ gaps) are padded with still
frames or silence, just as implicit gaps that are added automatically
to match track duration.

### Sequences

Parallels are put in _sequences_. The parallels within a sequence are
played sequentially; the first one is played in its entirety, then the
next one, and so on. This behaviour is different from how parallels
play their tracks. Parallels and sequences, with their different
playback behaviors, make up the fundamental building blocks of the
compositional editing in Komposition.

The following diagram shows a sequence of two parallels, playing
sequentially:

{% diagram :width => 600, :caption => "A sequence containing two parallels" %}
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Sequence (p1 :| [p2])
  # renderSequence defaultRenderSettings { containerLabels = True } (Id [])
 where
  p1 = 
    Parallel
      (Track Video [Clip 4 Nothing, Clip 3 Nothing])
      (Track Audio [Gap Explicit 2 Nothing, Clip 7 Nothing])
  p2 = 
    Parallel
      (Track Video [Gap Explicit 2 Nothing, Clip 8 Nothing])
      (Track Audio [Clip 7 Nothing])
{% enddiagram %}

### The Timeline

Finally, at the top level, we have the _timeline_. Effectively, the
timeline is a sequence of sequences; it plays every child sequence in
sequence. The reason for this level to exist is for the ability to
group larger chunks of a screencast within separate sequences.

{% diagram :width => 750, :caption => "A timeline containing two sequences, with two parallels
each" %}
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Timeline (s1 :| [s2])
  # renderTimeline defaultRenderSettings { containerLabels = True }
 where
  s1 = Sequence (p11 :| [p12])
  s2 = Sequence (p21 :| [p22])
  p11 = 
    Parallel
      (Track Video [Clip 2 Nothing, Clip 1 Nothing])
      (Track Audio [Gap Explicit 1 Nothing, Clip 3 Nothing])
  p12 = 
    Parallel
      (Track Video [Gap Explicit 1 Nothing, Clip 4 Nothing])
      (Track Audio [Clip 3 Nothing])
  p21 = 
    Parallel
      (Track Video [Clip 2 Nothing, Clip 1 Nothing, Clip 1 Nothing])
      (Track Audio [Gap Explicit 2 Nothing, Clip 1 Nothing])
  p22 = 
    Parallel
      (Track Video [Clip 4 Nothing])
      (Track Audio [Clip 2 Nothing, Gap Explicit 2 Nothing])
{% enddiagram %}

I use separate sequences within the timeline to delimit distinct parts
of a screencast, such as the introduction, the different chapters, and
the summary.

## Timeline Flattening

Komposition currently uses [FFmpeg](https://ffmpeg.org/) to render the
final media. This is done by constructing an `ffmpeg` command
invocation with a [filter
graph](https://ffmpeg.org/ffmpeg-filters.html) describing how to fit
together all clips, still frames, and silent audio parts.

FFmpeg doesn't know about hierarchical timelines; it only cares about
video and audio streams. To convert the hierarchical timeline into a
suitable representation to build the FFmpeg filter graph from,
Komposition performs _timeline flattening_.

The flat representation of a timeline contains only two tracks; audio
and video. All gaps are _explicitly_ represented in those tracks. The
following graph shows how a hierarchical timeline is flattened into
two tracks.

{% diagram :width => 750, :caption => "Timeline flattening transforming a hierarchical timeline" %}
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = tl === strutY 1 === scaleY 5 (arrowV (0 ^& (-1))) === flat
 where
  settings = defaultRenderSettings { containerLabels = True } 
  tl =
    renderTimeline settings (Timeline (s1 :| [s2]))
    # center
  flat =
    FlatTimeline
      (Track Video [ Clip 2 Nothing, Clip 1 Nothing, Gap Implicit 1 Nothing
                   , Gap Explicit 1 Nothing, Clip 4 Nothing
                   , Clip 2 Nothing, Clip 1 Nothing, Clip 1 Nothing
                   , Clip 4 Nothing
                   ])
      (Track Audio [ Gap Explicit 1 Nothing , Clip 3 Nothing
                   , Clip 3 Nothing, Gap Implicit 2 Nothing
                   , Gap Explicit 2 Nothing, Clip 1 Nothing, Gap Implicit 1 Nothing
                   , Clip 2 Nothing, Gap Explicit 2 Nothing
                   ])
    # renderFlatTimeline settings
    # center
  s1 = Sequence (p11 :| [p12])
  s2 = Sequence (p21 :| [p22])
  p11 = 
    Parallel
      (Track Video [Clip 2 Nothing, Clip 1 Nothing])
      (Track Audio [Gap Explicit 1 Nothing, Clip 3 Nothing])
  p12 = 
    Parallel
      (Track Video [Gap Explicit 1 Nothing, Clip 4 Nothing])
      (Track Audio [Clip 3 Nothing])
  p21 = 
    Parallel
      (Track Video [Clip 2 Nothing, Clip 1 Nothing, Clip 1 Nothing])
      (Track Audio [Gap Explicit 2 Nothing, Clip 1 Nothing])
  p22 = 
    Parallel
      (Track Video [Clip 4 Nothing])
      (Track Audio [Clip 2 Nothing, Gap Explicit 2 Nothing])
{% enddiagram %}

Notice in the graphic above how the implicit gaps at the ends of video
and audio tracks get represented with explicit gaps in the flat
timeline. This is because FFmpeg does not know how to render implicit
gaps. All gaps are represented explicitly, and are converted to clips
of still frames or silent audio when rendered with FFmpeg.

## Property Tests

To test the timeline flattening, there's a number of properties that
are checked. I'll go through each one and their property test code.

These properties were primarily written after I already had an
implementation. They capture some general properties of flattening
that I've come up with. In other cases, I've written properties before
beginning on an implementation, or to uncover an existing bug that
I've observed.

Thinking about your system's general behaviour and expressing that as
executable property tests is hard. I believe, like with any other
skill, that it requires a lot of practice. Finding general patterns
for properties, like the ones Scott Wlaschin describe in [Choosing
properties for property-based
testing](https://fsharpforfunandprofit.com/posts/property-based-testing-2/),
is a great place to start. When you struggle with finding properties
of your system under test, try applying these patterns and see which
work for you.

### Property: Duration Equality

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

```{.haskell}
hprop_flat_timeline_has_same_duration_as_hierarchical =
  property $ do
    -- 1. Generate a timeline with video clips in each parallel
    timeline' <- forAll $
        Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
  
    -- 2. Flatten the timeline and extract the result
    let Just flat = Render.flattenTimeline timeline'
    
    -- 3. Check that hierarchical and flat timeline duration are equal
    durationOf AdjustedDuration timeline'
      === durationOf AdjustedDuration flat
```

It generates a timeline using `forAll` and custom generators
(1). Instead of generating timelines of _any_ shape and filtering out
only the ones with video clips in each parallel, which would be very
inefficient, this test uses a custom generator to only obtain inputs
that satisfy the invariants of the system under test.

The range passed as the first argument to `Gen.timeline` is used as
the bounds of the generator, such that each level in the generated
hierarchical timeline will have at most 5 children.

`Gen.timeline` takes as its second argument _another generator_, the
one used to generate parallels, which in this case is
`Gen.parallelWithClips`.  With Hedgehog generators being regular
values, it's practical to compose them like this. A "higher-order
generator" can be a regular function taking other generators as
arguments.

As you might have noticed in the assertion (3), `durationOf` takes as
its first argument a value `AdjustedDuration`. What's that about?
Komposition supports adjusting the playback speed of video media for
individual clips. To calculate the final duration of a clip, the
playback speed needs to taken into account. By passing
`AdjustedDuration` we take playback speed into account for all video
clips.

#### Sidetrack: Finding a Bug

Let's say I had introduced a bug in timeline flattening, in which all
video gaps weren't added correctly to the flat video tracks. The
flattening is implemented as a fold, and it would not be unthinkable
that the accumulator was incorrectly constructed in a case. The test
would catch this quickly and present us with a minimal
counter-example:

![Hedgehog presenting a minimal counter-example](/assets/property-based-testing-the-ugly-parts/timeline-duration-failure.png){width=100%}

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
printed _size_ and _seed_ values to deterministically rerun the test
with the exact same inputs.

## Property: Clip Occurence

Slightly more complicated than the duration equality property, the
clip occurrence property checks that all clips from the hierarchical
timeline, and no other clips, occur within the flat timeline. As
discussed in the introduction on timeline flattening, implicit gaps
get converted to explicit gaps and thereby add more gaps, but no
video or audio clips should be added or removed.

```{.haskell}
hprop_flat_timeline_has_same_clips_as_hierarchical =
  property $ do
    -- 1. Generate a timeline with video clips in each parallel
    timeline' <- forAll $
        Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
    
    -- 2. Flatten the timeline
    let flat = Render.flattenTimeline timeline'
    
    -- 3. Check that all video clips occur in the flat timeline
    flat ^.. _Just . Render.videoParts . each . Render._VideoClipPart
        === timelineVideoClips timeline'
    
    -- 4. Check that all audio clips occur in the flat timeline
    flat ^.. _Just . Render.audioParts . each . Render._AudioClipPart
        === timelineAudioClips timeline'
```

The hierarchical timeline is generated and flattened like before (1,
2). The two assertions check that the respective video clips (3) and
audio clips (4) are equal. It's using lenses to extract clips from the
flat timeline, and the helper functions `timelineVideoClips` and
`timelineAudioClips` to extract clips from the original hierarchical
timeline.

## Still Frames Used

In the process of flattening, the still frame source for each gap is
selected. It doesn't assign the actual pixel data to the gap, but a
value describing which asset the still frame should be extracted from,
and whether to pick the first or the last frame (known as _still frame
mode_.) This representation lets the flattening algorithm remain a
pure function, and thus easier to test. Another processing step runs
the effectful action that extracts still frames from video files on
disk.

The decision of still frame mode and source is made by the flattening
algorithm based on the parallel in which each gap occur, and what
video clips are present before or after. It favors using clips
occurring after the gap. It only uses frames from clips before the gap
in case there are no clips following it. To test this behaviour, I've
defined three properties.

### Property: Single Initial Video Clip

The following property checks that an initial single video clip,
followed by one or more gaps, is used as the still frame source for
those gaps.

```{.haskell}
hprop_flat_timeline_uses_still_frame_from_single_clip =
  property $ do
    -- 1. Generate a video track generator where the first video part
    --    is always a clip
    let genVideoTrack = do
          v1 <- Gen.videoClip
          vs <- Gen.list (Range.linear 1 5) Gen.videoGap
          pure (VideoTrack () (v1 : vs))
  
    -- 2. Generate a timeline with the custom video track generator
    timeline' <- forAll $ Gen.timeline
      (Range.exponential 0 5)
      (Parallel () <$> genVideoTrack <*> Gen.audioTrack)
  
    -- 3. Flatten the timeline
    let flat = Render.flattenTimeline timeline'
  
    -- 4. Check that any video gaps will use the last frame of a 
    --    preceding video clip
    flat
      ^.. ( _Just
          . Render.videoParts
          . each
          . Render._StillFramePart
          . Render.stillFrameMode
          )
      &   traverse_ (Render.LastFrame ===)
```

The custom video track generator (1) always produces tracks with an
initial video clip followed by one or more video gaps. The generated
timeline (2) can contain parallels with any audio track shape, which
may result in a _longer_ audio track and thus an implicit gap at the
end of the video track. In either case, all video gaps should padded
with the last frame of the initial video clip, which is checked in the
assertion (4).

{% diagram :width => 375, :caption => "Still frames being sourced from the single initial video clip" %}
import           Diagrams.Direction (dir)
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Parallel
    (Track Video [Clip 4 (Just "Video Clip 1"), Gap Explicit 2 Nothing, Gap Explicit 3 Nothing])
    (Track Audio [Clip 9 (Just "Audio Clip 1")])
  # renderParallel defaultRenderSettings { parallelArrows = False } (Id [])
  # (strutY 1 ===)
  # connectParts arrowRot (Id [1, 2]) (Id [1, 1])
  # connectParts arrowRot (Id [1, 3]) (Id [1, 1])
  where
    arrowRot = 1/6 @@ turn
{% enddiagram %}

### Property: Ending with a Video Clip

In case the video track ends with a video clip, and is longer than the
audio track, all video gaps within the track should use the first
frame of a following clip.

```{.haskell}
hprop_flat_timeline_uses_still_frames_from_subsequent_clips =
  property $ do
    -- 1. Generate a parallel where the video track ends with a video clip,
    --    and where the audio track is shorter
    let
      genParallel = do
        vt <-
          VideoTrack ()
            <$> (   snoc
                <$> Gen.list (Range.linear 1 10) Gen.videoPart
                <*> Gen.videoClip
                )
        at <- AudioTrack () . pure . AudioGap () <$> Gen.duration'
          (Range.linearFrac
            0
            (durationToSeconds (durationOf AdjustedDuration vt) - 0.1)
          )
        pure (Parallel () vt at)
  
    -- 2. Generate a timeline with the custom parallel generator
    timeline' <- forAll $ Gen.timeline (Range.exponential 0 5) genParallel
  
    -- 3. Flatten the timeline
    let flat = Render.flattenTimeline timeline'
  
    -- 4. Check that all gaps use the first frame of subsequent clips
    flat
      ^.. ( _Just
          . Render.videoParts
          . each
          . Render._StillFramePart
          . Render.stillFrameMode
          )
      &   traverse_ (Render.FirstFrame ===)
```

The custom generator (1) produces parallels where the video track is
guaranteed to end with a clip, and where the audio track is 100 ms
shorter than the video track. This ensures that there's no implicit
video gap at the end of the video track. Generating (2) and flattening
(3) is otherwise the same as before. The assertion (4) checks that all
video gaps uses the first frame of a following clip.

{% diagram :width => 600, :caption => "Still frames being sourced from following video clips when possible" %}
import           Diagrams.Direction (dir)
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Parallel
    (Track Video [Clip 3 (Just "Video Clip 1"), Gap Explicit 1 Nothing, Clip 3 (Just "Video Clip 2"), Gap Explicit 1.5 Nothing, Gap Explicit 2.5 Nothing, Clip 4 (Just "Video Clip 3")])
    (Track Audio [Clip 10 (Just "Audio Clip 1")])
  # renderParallel defaultRenderSettings { parallelArrows = False } (Id [])
  # (strutY 1 ===)
  # connectParts arrowRot (Id [1, 2]) (Id [1, 3])
  # connectParts arrowRot (Id [1, 4]) (Id [1, 6])
  # connectParts arrowRot (Id [1, 5]) (Id [1, 6])
  where
    arrowRot = (-1/6) @@ turn
{% enddiagram %}

### Property: Ending with an Implicit Video Gap

The last property on still frame usage covers the case where the video
track is shorter than the audio track. This leaves an implicit gap
which, just like explicit gaps inserted by the user, are padded with
still frames.

```{.haskell}
hprop_flat_timeline_uses_last_frame_for_automatic_video_padding =
  property $ do
    -- 1. Generate a parallel where the video track only contains a video
    --    clip, and where the audio track is longer
    let
      genParallel = do
        vt <- VideoTrack () . pure <$> Gen.videoClip
        at <- AudioTrack () . pure . AudioGap () <$> Gen.duration'
          (Range.linearFrac
            (durationToSeconds (durationOf AdjustedDuration vt) + 0.1)
            10
          )
        pure (Parallel () vt at)
  
    -- 2. Generate a timeline with the custom parallel generator
    timeline' <- forAll $ Gen.timeline (Range.exponential 0 5) genParallel
  
    -- 3. Flatten the timeline
    let flat = Render.flattenTimeline timeline'
  
    -- 4. Check that video gaps (which should be a single gap at the
    --    end of the video track) use the last frame of preceding clips
    flat
      ^.. ( _Just
          . Render.videoParts
          . each
          . Render._StillFramePart
          . Render.stillFrameMode
          )
      &   traverse_ (Render.LastFrame ===)
```

The custom generator (1) generates a video track consisting of video
clips only, and an audio track that is 100ms longer. Generating the
timeline (2) and flattening (3) are again similar to the previous
property tests. The assertion (4) checks that all video gaps use the
last frame of preceding clips, even if we know that there should only
be one at the end.

{% diagram :width => 350, :caption => "Still frames being sourced from preceding video clip for last implicit gap" %}
import           Diagrams.Direction (dir)
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Parallel
    (Track Video [Clip 5 (Just "Video Clip 1"), Gap Implicit 3 Nothing])
    (Track Audio [Clip 8 (Just "Audio Clip 1")])
  # renderParallel defaultRenderSettings { parallelArrows = False } (Id [])
  # (strutY 1 ===)
  # connectParts arrowRot (Id [1, 2]) (Id [1, 1])
  where
    arrowRot = 1/6 @@ turn
{% enddiagram %}

## Properties: Flattening Equivalences

The last property I want to show in this case study checks flattening
at the sequence and parallel levels. While rendering a full project
always flattens at the timeline, the _preview_ feature in Komposition
can be used to render and preview a single sequence or parallel.

There should be no difference between flattening an entire timeline
and flattening all of its sequences or parallels and folding those
results into a single flat timeline. This is what the _flattening
equivalences_ properties are about.

```haskell
hprop_flat_timeline_is_same_as_all_its_flat_sequences =
  property $ do
    -- 1. Generate a timeline
    timeline' <- forAll $
      Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
  
    -- 2. Flatten all sequences and fold the resulting flat
    --    timelines together
    let flat = timeline' ^.. sequences . each
               & foldMap Render.flattenSequence
  
    -- 3. Make sure we successfully flattened the timeline
    flat /== Nothing
               
    -- 4. Flatten the entire timeline and compare to the flattened 
    --    sequences
    Render.flattenTimeline timeline' === flat
```

The first property generates a timeline (1) where all parallels have
at least one video clip. It flattens all sequences within the timeline
and folds the results together (2). Folding flat timelines together
means concatenating their video and audio tracks, resulting in a
single flat timeline.

Before the final assertion, it checks that we got a result (3) and not
`Nothing`. As it's using the `Gen.parallelWithClips` generator there
should always be video clips in each parallel, and we should always
successfully flatten and get a result. The final assertion (4) checks
that rendering the original timeline gives the same result as the
folded-together results of rendering each sequence.

The other property is very similar, but operates on parallels rather
than sequences:

```haskell
hprop_flat_timeline_is_same_as_all_its_flat_parallels =
  property $ do
    -- 1. Generate a timeline
    timeline' <- forAll $
      Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
  
    -- 2. Flatten all parallels and fold the resulting flat
    --    timelines together
    let flat = timeline' ^.. sequences . each . parallels . each
               & foldMap Render.flattenParallel
  
    -- 3. Make sure we successfully flattened the timeline
    flat /== Nothing
  
    -- 4. Flatten the entire timeline and compare to the flattened 
    --    parallels
    Render.flattenTimeline timeline' === flat
```

The only difference is in the traversal (2), where we apply
`Render.flattenParallel` to each parallel instead of applying
`Render.flattenSequence` to each sequence.


## Missing Properties

Whew! That was quite a lot of properties and code, especially for a
warm-up. But timeline flattening could be tested more thoroughly! I
haven't yet written the following properties, but I'm hoping to find
some time to add them:

- **Clip playback timestamps are the same.** The
  "clip occurrence" property only checks that the hierarchical
  timeline's clips occur in the flat timeline. It doesn't check _when_
  in the flat timeline they occur. One way to test this would be to
  first annotate each clip in original timeline with its playback
  timestamp, and transfer this information through to the flat
  timeline. Then the timestamps could be included in the assertion.
  
- **Source assets used as still frame sources.** The "still frames
  used" properties only check the still frame _mode_ of gaps, not
  the still frame _sources_. The algorithm could have a bug where it
  always uses the first video clip's asset as a frame source, and the
  current property tests would not catch it.
  
- **Same flat result is produced regardless of sequence grouping.**
  Sequences can be split or joined in any way without affecting the
  final rendered media. They are merely ways of organizing parallels
  in logical groups. A property could check that however you split or
  join sequences within a timeline, the flattened result is the same.

## A Missing Feature

As pointed out earlier, parallels must have at least one video
clip. The flattening algorithm can only locate still frame sources for
video gaps from within the same parallel. This is an annoying
limitation when working with Komposition, and the algorithm should be
improved.

As the existing set of properties describe timeline flattening fairly
well, changing the algorithm could be done with a TDD-like
workflow:

1. Modify the property tests to capture the intended behaviour
2. Tests will fail, with the errors showing how the existing
   implementation fails to find still frame sources as expected
3. Change the implementation to make the tests pass

PBT is not only an after-the-fact testing technique. It can be used
much like conventional example-based testing to drive development.

## Obligatory Cliff-Hanger

In this post we've looked at timeline flattening, the simplest case
study in the "Property-Based Testing in a Screencast Editor" series.
The system under test was a module of pure functions, with complex
enough behaviour to showcase PBT as a valuable tool. The tests are
more closely related to the design choices and concrete
representations of the implementation.

Coming case studies will dive deeper into the more complex subsystems
of Komposition, and finally we'll see how PBT can be used for
integration testing. At that level, the property tests are less tied
to the implementation, and focus on describing the higher-level
outcomes of the interaction between subsystems.

Next up is property tests for the video classifier. It's also
implemented a pure function, but with slightly more complicated logic
that is trickier to test. We're going to look at an interesting
technique where we generate the _expected output_ instead of the
input.

1. [Introduction](/programming/2019/03/02/property-based-testing-in-a-screencast-editor-introduction.html)
1. **Timeline Flattening**
1. [Video Scene Classification](/programming/2019/04/17/property-based-testing-in-a-screencast-editor-case-study-2.html)
1. [Integration Testing](/programming/2019/06/02/property-based-testing-in-a-screencast-editor-case-study-3.html)

Thanks for reading! See you next time.

## Credits

Thank you Chris Ford and Ulrik Sandberg for proof-reading and giving
valuable feedback on drafts of this post.

## Footnotes

[^1]: Final Cut Pro has [compound
clips](https://support.apple.com/kb/PH12631?locale=en_US), and Adobe
Premiere Pro has [nested
sequences](https://www.premiumbeat.com/blog/nesting-in-adobe-premiere-pro/).
[^2]: The [section on
workflow](https://owickstrom.github.io/komposition/user-guide/workflow/)
in Komposition's documentation describes how to plan, record, and edit
your screencast in way compatible with Komposition.
