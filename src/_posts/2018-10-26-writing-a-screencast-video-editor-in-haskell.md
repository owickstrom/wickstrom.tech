---
layout: post
title: Writing a Screencast Video Editor in Haskell
author: Oskar Wickström
categories: programming
draft: true
tags: ["haskell", "video", "screencast", "editor", "gtk", "functional"]
excerpt: |
  In dire need of better tools when producing screencasts for *Haskell at
  Work*, I started building *Komposition*, the video editor for screencasters.
  This desktop application automatically detects scenes in screen capture
  video, detects sentences in audio parts, and features and a high-productivity
  editing workflow keyboard-driven navigation.
---

For the last six months I've been working on a screencast video editor called
*Komposition*, and it's now released and open source. This post is an
experience report based on a talk from Lambda World Cádiz 2018, that'll give an
overview of its design, implementation, testing, and planned future work.

## Background

It all began with [Haskell at Work](https://haskell-at-work.com), the series of
screencasts focused on practical Haskell that I've been producing the last
year. The screencasts are fast-paced tutorials centered around the terminal
our text editor, complemented by a voice-over audio track.

!["The evolution of a functional programmer," based on [Human evolution scheme by M. Garde - Self work (Original by: José-Manuel Benitos), CC BY-SA 3.0](https://commons.wikimedia.org/w/index.php?curid=2165296)](/assets/writing-a-screencast-video-editor-in-haskell/evolution.png)

My workflow for producing these screencasts consists of:

1. Writing a very detailed script. The script usually gets uploaded to the
   website as-is, being used as the show notes for the screencast.
2. Recording video separately, including all mistakes and retries, in a single
   video file. Each little part of editing or running commands in the terminal
   is separated by a rest, where I don't type anything at all for a few
   seconds.
3. Recording audio separately, where I do the voiceover track based on the
   script. A mistake or mispronounciation is corrected by taking a small break
   and then trying the same sentence or paragraph again.
4. Cut and arrange all parts of the screencast using a video editor. This is
   the most painful and time-consuming part of the process.

I've found this workflow beneficial for me, partly because of being a
non-native English speaker and not being keen on coding and talking
simultaneously, but also because I think it helps with organizing the content
into a cohesive narrative. I can write the script almost as a text-based
tutorial, and read it through to make sure it flows well, and then go into
recording. Having to redo the recording phase is _very_ time consuming, so I'm
putting a lot of effort into the script phase to avoid catching mistakes late.

### Video Editors

I've tried a bunch of video editors. First, I tried free software alternatives,
including Kdenlive, OpenShot, and a few more. Unfortunately, the audio
effects available were a bit disappointing. I use at least normalization and
a noise gate, but prefer to also have a good compressor.

More importantly, though, these applications are built for all kinds of video
editing, and they are optimized for that purpose. I'm using a very small subset
of their feature set, and that subset is not suited to my editing workflow.
While it works, the tasks are repetitive and time-consuming.

On the commercial side, there are applications like Premiere Pro and Final Cut
Pro. These are proprietary and expensive systems, but they offer very good
effects and editing capabilities. I used Premiere Pro for a while, and while I
enjoyed the stability and quality of the tools, I still suffered from the
repetitive workload of cutting and organizing the video and audio clips to form
my screencasts.

![[Yak by travelwayoflife - Flickr, CC BY-SA 2.0](https://commons.wikimedia.org/w/index.php?curid=22106967)](/assets/writing-a-screencast-video-editor-in-haskell/yak.jpg)

What does a programmer do when faced with a repetitive task? *Spend way more
time on automating it!* Thus, I started what came to be the greatest yak shave
of my life. So far...

### Building a Screencast Video Editor

I decided to build a screencast video editor tailored to my workflow, and one
that is minimal in its feature set, doing only this kind of editing, but
_really well_.

You might think "why not extend the free software editors to cover your needs?"
That is a fair question. I wanted to rethink the editing experience, starting
with a blank slate, and question the design choices made in the traditional
systems. Also, to be honest, I'm not so keen on using my spare time to write
C++.

I decided to write it in Haskell, using GTK+ for the graphical user interface.
Another option would be Electron and PureScript, but the various horror stories
about Electron memory usage, in combination with it running on NodeJS, made me
decide against it. As I expected my application to perform some
performance-critical tasks around video and audio processing, Haskell seemed
like the best choice of the two. There are many other languages and frameworks
that could've been used, but this combination fit me well.

## Komposition

About five months later, after many hours of hacking, _Komposition_ was
[released open-source under the Mozilla Public License
2.0](https://github.com/owickstrom/komposition). While the working name was
_FastCut_, when releasing I renamed it to the the Swedish word for
"composition." It has nothing to do with KDE.

Komposition is a modal, cross-platform, GUI application. The modality means
that you're always in exactly one mode, and that only certain actions can be
taken depending on the mode. At the heart of the editing model lies the
hiearchical timeline, which we'll dive into shortly.

Other central features of Komposition include the automatic video and audio
classification tools. They automate the tedious task of working through your
recorded video and audio files to cut out the interesting parts. After
importing, you'll have a collection of classified video scenes, and a
collection of classified audio parts. The audio parts are usually sentences,
but it depends on how you take rests when recording your voiceover.

### Keyboard-Driven Editing

Finally, Komposition is built for keyboard-driven editing, currently with
Vim-like bindings, and commands transforming the hierarchical timeline,
inspired by Paredit for Emacs.

![Keybinding help dialog](/assets/writing-a-screencast-video-editor-in-haskell/keybindings.png){width=345px}

There are corresponding menu items for most commands, and there's limited
support for using the mouse in the timeline. If you need help with keybindings,
press the question mark key, and you will be presented with a help dialog
showing the bindings available in the current mode.

### The Hierarchical Timeline

The hiearchical timeline is a tree structure with a fixed depth.  At the leafs
of the tree there are _clips_. Clips are placed in the video and audio tracks
of a _parallel_. It's named that way because the video and audio tracks play in
parallel. Clips within a track play in sequence.

![Video and audio tracks play in parallel](/assets/writing-a-screencast-video-editor-in-haskell/timeline1.svg)

If the audio track is longer than the video track, the remaning part of the
video track is padded with still frames from an adjacent clip.

![Shorter video tracks are automatically padded with still frames](/assets/writing-a-screencast-video-editor-in-haskell/timeline2.svg)

Explicit _gaps_ can be to the video and audio tracks. Video gaps are padded with
still frames, and audio gaps are silent. When you add the gap you specify its
duration.

![Gaps can be added in video and audio tracks](/assets/writing-a-screencast-video-editor-in-haskell/timeline3.svg)

Parallels are put in _sequences_, and they are played in sequence. The first
parallel is played until its end, then the next is played, and so on.
Parallels and sequences are used to group cohesive parts of your screencast,
and to synchronize the start of related video and audio clips.


![Parallels are played in sequence](/assets/writing-a-screencast-video-editor-in-haskell/timeline4.svg)

When editing within a sequence or parallel, for example when deleting or adding
clips, you will not affect the synchronization of other sequences or parallels.
If there were only audio and video tracks in Komposition, deleting an audio
clip would possibly shift many other audio clips, causing them to get out of
sync with their related video clips. This is why the timeline structure is
built up using sequences and parallels.

Finally, the _timeline_ is the top-level structure that contains sequences.
This is merely for organizing larger parts of a screencast. You can comfortably
build up your screencast with a single sequence containing parallels.

![The timeline contains sequences that are played in sequence](/assets/writing-a-screencast-video-editor-in-haskell/timeline5.svg)

Note that the timeline always contains at least one sequence, and that all
sequences contain at least one parallel. The tracks within a parallel can be
empty, though.

### Documentation

[The project website](https://owickstrom.github.io/komposition/) includes a
user guide, covering the concepts of the application, along with
recommendations on how to plan and record your screencasts to achieve the best
results and experience using Komposition.

![Komposition's user guide](/assets/writing-a-screencast-video-editor-in-haskell/documentation.png)

The landing page features a tutorial screencast, explaining how to import,
edit, and render a screencast. It's already a bit outdated, but I might get
around to making an updated version when doing the next release. Be sure to
check it out, it'll give you a feel for how editing with Kompositoin works.

I can assure you, editing a screencast, that is about editing screencasts
using your screencast editor, in your screencast editor, is _quite the
mind-bender_. And I thought I had recursion down.

## Implementation

I've strived to keep the core domain code in Komposition pure. That is, only
pure function and data structures. Currently, the timeline and focus, command
and event handling, key bindings, and the video classification algorithm are
all pure. There are still impure parts, like audio and video import, audio
classification, preview frame rendering, and the main application control flow.

Some parts are inheritely effectful, so it doesn't make sense to try writing
them as pure functions, but as soon as the complexity increases it's worth
considering what can be separated out as pure functions. The approach of
"Functional core, imperative shell" (Boundaries, Gary Bernhardt) describes this
style very well.  If you can do this for the complex parts of your program, you
have a much starting point for automated testing, something I'll cover later in
this post.

### GTK+

Komposition's GUI is built with GTK+ 3 and the Haskell bindings from `gi-gtk`.
I noticed early on the project that the conventional programming style and the
APIs of GTK+ were imperative, callback-oriented, and all operating within `IO`,
making it painful to use from Haskell, especially while trying to keep complex
parts of the application free of effects.

To mitigate this issue, I started building a library (more yak shaving!) called
`gi-gtk-declarative`, which is a declarative layer on top of `gi-gtk`.  The
[previous post in this
blog](/programming/2018/09/04/declarative-gtk-programming-with-haskell.html)
the project in detail. Using the declarative layer, rendering becomes a pure
function `(state -> Widget event)`, where `state` and `event` varies with
the mode and view that's being rendered. Event handling is based on values
and pure functions.

There are cases where custom widgets and exceptions are needed, calling the
imperative APIs of `gi-gtk`, but they are well-isolated and few in numbers.

### Type-Indexed State Machines

I had a curiousity itching when starting this project, that I decided to
scratch. Last year I worked on porting [the Idris ST
library](http://docs.idris-lang.org/en/latest/st/), providing a way to encode
type-indexed state machines in GHC Haskell. The library is called
[Motor](http://hackage.haskell.org/package/motor).

Just to give some short examples, the following type signatures, used in the
main application control flow, operate on the application state machine
that's parameterized by its mode.

The `start` function takes a name and keymaps, and creates a new application
state machine associated with the name, and in the state of
`WelcomeScreenMode`:

```haskell
start
	:: Name n
	-> KeyMaps
	-> Actions m '[ n !+ State m WelcomeScreenMode] r ()
```

The `returnToTimeline` function takes a name of an existing state machine and a
`TimelineModel`, and transitions the application from the current mode to
`TimelineMode`, given that the current mode instantiates the
`ReturnsToTimeline` class:

```haskell
returnToTimeline
	:: ReturnsToTimeline mode
	=> Name n
	-> TimelineModel
	-> Actions m '[ n := State m mode !--> State m TimelineMode] r ()
```

The usage of Motor in Komposition is likely the most complicated aspect of the
codebase, and I have been hesitating if it is worth the complexity. On the
positive side, combining this with GADTs and the singleton pattern for
mode-specific commands and events, GHC can really help out with
pattern-matching and exhaustivity-checking. No nasty `(error "this shouldn't
happen")` as the fall-through case when pattern matching!

I'm currently in the process of rewriting much of the state machine encoding in
Komposition, using it more effectively for managing windows and modals in GTK+,
and I think this warrants the use of Motor more clearly. Otherwise, it might be
worth falling back to a less advanced encoding, like the one I described
[Finite-State Machines, Part 2: Explicit Typed State
Transitions](/finite-state-machines/2017/11/19/finite-state-machines-part-2.html).

### Singleton Pattern

The _singleton pattern_ is used in a few places in Komposition, as mentioned
above. To show a concrete example, the encoding of mode-specific commands and
events is based on the `Mode` data type.

```haskell
data Mode
  = WelcomeScreenMode
  | TimelineMode
  | LibraryMode
  | ImportMode
```

This type is lifted to the kind level using the `DataKinds` language extension,
and is used in the corresponding definition of the singleton `SMode`.

```haskell
data SMode m where
  SWelcomeScreenMode :: SMode WelcomeScreenMode
  STimelineMode      :: SMode TimelineMode
  SLibraryMode       :: SMode LibraryMode
  SImportMode        :: SMode ImportMode
```

The `Command` data type is parameterized by the mode in which the command is
valid. Some commands are valid in all modes, like `Cancel` and `Help`, while
others are specific to a certain mode. `FocusCommand` and `JumpFocus` are only
valid in the timeline mode, as seen in their type signatures below.

```haskell
data Command (mode :: Mode) where
  Cancel       :: Command mode
  Help         :: Command mode
  FocusCommand :: FocusCommand -> Command TimelineMode
  JumpFocus    :: Focus SequenceFocusType -> Command TimelineMode
  -- ...
```

Finally, by passing a singleton for a mode to the `keymaps` function, we get
back a keymap specific to that mode. This is used to do event handling and key
bindings generically for the entire application.

```haskell
keymaps :: SMode m -> KeyMap (Command m)
keymaps =
  \case
    SWelcomeScreenMode ->
      [ ([KeyChar 'q'], Mapping Cancel)
      , ([KeyEscape], Mapping Cancel)
      , ([KeyChar '?'], Mapping Help)
      ]
    -- ...
```

In the spirit of calling out usage of advanced GHC features, I think singletons
and GADTs are one more such instance. However, I find them very useful in this
context, and worth the added cognitive load. Worth noting, you don't have to go
full "Dependent Haskell" or bring in the singletons library to leverage some of
these techniques.

### Automatic Scene Classification

The automatic classification of scenes in video is implemented using the
[Pipes](http://hackage.haskell.org/package/pipes) and
[ffmpeg-light](http://hackage.haskell.org/package/ffmpeg-light) libraries,
mainly. It begins with the `readVideoFile`, that given a video file path will
give us a `Pipes.Producer` of timed frames, which are basically pixel arrays
tagged with their time in the original video. The producer will stream the
video file, and yield timed frames as they are consumed.

```haskell
readVideoFile :: MonadIO m => FilePath -> Producer (Timed Frame) m ()
```

The frames are converted from
[JuicyPixels](http://hackage.haskell.org/package/JuicyPixels) frames to
[massiv](http://hackage.haskell.org/package/massiv) frames, and then the
producer is passed to the `classifyMovement` function. Given a minimum segment
duration (such that segments cannot be shorter than *N* seconds) and a producer
of timed frames, it returns a producer of `Classified` frames, tagging each
frame as being either moving or still.

```haskell
classifyMovement
		:: Monad m
		=> Time -- ^ Minimum segment duration
		-> Producer (Timed RGB8Frame) m ()
		-> Producer (Classified (Timed RGB8Frame)) m ()

data Classified f
  = Moving f
  | Still f
  deriving (Eq, Functor, Show)
```

Finally, the `classifyMovingScenes` function, given a full duration of the
origina video and a producer of classified frames, returns a producer of
`ProgressUpdate` values, and eventually returns a list of `TimeSpan` values.

```haskell
classifyMovingScenes ::
			Monad m
	=> Duration -- ^ Full length of video
	-> Producer (Classified (Timed RGB8Frame)) m ()
	-> Producer ProgressUpdate m [TimeSpan]
```

The timespans describe which parts of the original video are considered moving
scenes, and the progress update values are used to render a progress bar in
the GUI as the classification is running.

### Automatic Sentence Classification

Similar to the video classification, Komposition also classifies audio files to
find the sentences or paragraphs in your voiceover audio. The implementation
relies heavily on the `sox` tool, a separate executable that's used to do:

- Normalization
- Noise gate
- Auto-splitting by silence

The problematic part with `sox` is that it (as far as I know) can only write
the split audio files to disk. I haven't found a way to retrieve the timespans
in the original audio file, so that information is unfortunately lost. This
will become more appearent when Komposition supports editing the start and end
position of clips, as it can't be supported for audio clips produced by `sox`.

I hope to find some way around this, by extending or parsing output from `sox`
somehow, by using `sox` as a library and writing bindings, or by implementing
the audio classification in Haskell. I'm trying to avoid the last alternative.

### Rendering

The rendering pipeline begins with a pure function that converts the
hierarchical timeline to a flat timeline representation. This representation
consists of a video track and an audio track, where all gaps are made explicit,
and where the tracks are of equal duration.

From the flat representation, an FFmpeg command is built up. This is based on a
data type representation of the FFmpeg command-line syntax, and most
importantly the filter graph DSL that's used to build up the complex FFmpeg
rendering command.

Having an intermediate data type representation when building up complex
command invocatoins, instead of going directly to `Text` or `String`, is
something I highly recommend.

### Preview

The preview pipeline is very similar to the rendering pipeline. In fact, it's
using the same machinery, except for the output being a streaming HTTP server
instead of a file, and that it's passed the proxy media instead of the
full-resolution original video. On the other side there's a GStreamer widget
embedded in the GTK+ user interface that plays back the HTTP video stream.

Using HTTP might seem like a strange choice for IPC between FFmpeg and
GStreamer. Surprisingly, it's the option that have worked most reliably across
operating systems for me, but I'd like to find another IPC mechanism
eventually.

The HTTP solution is also somewhat unreliable, as I couldn't find
a way to ensure that the server is ready to stream, so there's a race condition
between the server and the GStreamer playback, silently "solved" with an ugly
`threadDelay`.

## Testing

Let's talk a bit about testing in Komposition. I've used multiple techniques,
but the one that stands out as unconventional, and specific to the domain of
this application, is the color-tinting video classifier.

![Output of the color-tinting video classifier](/assets/writing-a-screencast-video-editor-in-haskell/color-tinting.gif)

It uses the same classification functions as described before, but instead of
returning timespans, it creates a new video file where moving frames are tinted
green and still frames are tinted red. This tool made it much easier to tweak
the classifier and test it real recordings.

### Property-Based Testing

I've used Hedgehog to test some of the most complex parts of the codebase. This
has been incredibly valuable, and has found numerous errors and bad assumptions
in the implementation. Functionality tested with Hedgehog and properties
include:

- **Timeline commands and movement:** It generates a sequence of commands,
together with consistent and timeline and focus for that timeline. Then it
applies all commands to the current timeline and focus. After each applied
command, it checks that the timeline and focus are still consistent. This
checks that you don't have the possibility of out-of-bounds movement, or that
deleting or otherwise transforming the timeline causes an inconstitent timeline
and focus pair.
- **Video scene classification:** It generates known test scenes of random
durations, that are either scenes of only still frames, or scenes with moving
frames. It translates the test scenes, which are just descriptions, to real
frame, and runs the classifier on the frames. Finally, it checks that the
classified scenes are the same as the generated test scenes.
- **Flattening of hierchical timeline:** The flattening process described
before converts the hierarchical timeline to a flat representation. This test
checks that hierarchical and flat timelines are always of the same total
duration. It could be extended with other properties, like that all clips in
the original timeline are present in the flat timeline.
- **Roundtrip properties of FFmpeg format printers and parsers:** This is
conventional use of property-based tests. It ensures that parsing an
FFmpeg-format timestamp string, produced by the FFmpeg-format timestamp printer,
gives you back the same timestamp as you started with.

There are also cases of example-based testing, but I won't cover them in this
report.

## Used Packages

Komposition depends on a decent collection of Haskell and non-Haskell tools and
libraries to work with video, audio, and GUI rendering. I want to highlight
some of them.

### haskell-gi

The [haskell-gi](https://github.com/haskell-gi/haskell-gi) family of packages
are used extensively:

- gi-gobject
- gi-glib
- gi-gst
- gi-gtk
- gi-gdk
- gi-gdkpixbuf
- gi-pango

They supply bindings to GTK+, GStreamer, and more, primarily generated from the
GObject Introspection metadata. While GTK+ has been problematic to work with
in Haskell, these bindings have been crucial to the development of Komposition.

## massiv & massiv-io

The [massiv](http://hackage.haskell.org/package/massiv) package is an array
library that uses function composition to accomplish a sort of fusion. It's
used to do parallel pixel comparison in the video classifier. A huge thanks to
[Alexey Kuleshevich](https://github.com/lehins), the author and maintainer of
massiv, for helping me implement the first version!

## Pipes

The [Pipes](http://hackage.haskell.org/package/pipes) library is used
extensively in Komposition:

* The streaming video reader from [ffmpeg-light](http://hackage.haskell.org/package/ffmpeg-light) is wrapped in a `Pipes.Producer` to provide composable streaming.
* In general, effectful operations with progress notifications are producers that yield `ProgressUpdate` values as they perform their work.
* [pipes-safe](http://hackage.haskell.org/package/pipes-safe) is used for handling resources and processes.
* [pipes-parse](http://hackage.haskell.org/package/pipes-parse) is used in stateful transformations in the video classifier.

A big thanks to [Gabriel Gonzales](https://twitter.com/GabrielG439), the author
of Pipes and the related packages!

## Others

To name a few more:

- I've used [protolude](http://hackage.haskell.org/package/protolude) as the basis for a custom prelude.
- The [lens](http://hackage.haskell.org/package/lens) library is used for working with nested data structures, positional updates in lists, and monadic transformations.
- [typed-process](http://hackage.haskell.org/package/typed-process) is used where together with pipes-safe, in a situation where I couldn't use the regular [process](http://hackage.haskell.org/package/process) package because of version constraint issues. The typed-process API turned out to be really nice, so I think it will be used more in the future.

## Summary

Looking back at this project, the best part has been to first write it for my
own use, and later find out that quite a lot of people are interested in how
it's built, and even using it themselves. I've already received pull requests,
bug reports, usability feedback, and many kind words and encouragements. It's
been great!

Also, it's been working on an application that can be considered outside of
GHC's and Haskell's comfort zone, namely a multimedia and GUI application.
Komposition is not the first application to explore this space --- see
[Movie Monad](https://lettier.github.io/movie-monad/) and [Gifcurry](https://lettier.github.io/gifcurry/) for other examples --- but it is exciting,
nonetheless.

Speaking of using Haskell, the strive to keep complex domain logic free of
effects, and to use property-based testing with Hedgehog to lure out nasty
bugs, has been incredibly satisfying and a great learning experience.

### The Problematic Parts

It's not been all fun and games, though. I've spent many hours struggling with
FFmpeg, video and audio codecs, containers, and streaming. Executing external
programs and parsing their output has been time consuming and very hard to
test. GTK+ has been very valuable, but also difficult to work with in Haskell.
Finally, management of non-Haskell dependencies, in combination with trying to
be cross-platform, is painful. Nix has helped with my own setup, but everyone
will not install using Nix.

### Next Steps

There are many features that I'd like to add in the near future.

- More commands to work with the timeline, e.g. yank, paste, join, and more.
- Previewing of any timeline part. Currently you can only preview the entire
timeline, sequences, and parallels.
- Adjustable clips, meaning that you can change the timespan of a clip. This
is useful if the automatic classification generated slightly incorrect clip
timespans.
- Content-addressed project files, to enable reuse of generated files, and to
avoid collision. This includes most files involved in importing, and generated
preview frames.

It would be great to set up packaging for popular operating systems, so that
people can install Komposition without compiling from source. There's already
a Nix expression that can be used, but I'd like to supply Debian packages,
macOS bundles, Windows installers, and possibly more.

There are some things that I'd like to explore and assess, but that won't
necessarily happen. The first is to use GStreamer in rendering pipeline,
instead of FFmpeg. I think this is possible, but I haven't dug in. The second
thing, an idea that evolved when talking to people at Lambda World, would be to
use voice recognition on audio clips to show text in the preview area, instead
of showing a waveform.

Finally, there are some long-awaited refactorings and cleanups waiting, and
optimization of the FFmpeg filter graph and the diffing in gi-gtk-declarative.
Some of these I've already started on.

## Wrap-Up

I hope you enjoyed reading this report, and that you now have got a clearer
picture of Komposition, its implementation, and where it's going. If you're
interested in using it, [let me know how it works
out](https://gitter.im/owickstrom/komposition), and if you want to contribute
with reporting bugs or sending pull requests, there's [the issue tracker on
GitHub](github.com/owickstrom/komposition/issues).
