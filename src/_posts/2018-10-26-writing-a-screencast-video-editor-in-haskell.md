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

There are corresponding menu items for most commands, and there's limited
support for using the mouse in the timeline. If you need help with keybindings,
press the question mark key, and you will be presented with a help dialog
showing the bindings available in the current mode.

![Keybinding help dialog](/assets/writing-a-screencast-video-editor-in-haskell/keybindings.png){width=345px}

### Hierarchical Timeline{background=#dddddd}

## Clips

![Clips](images/timeline1.svg){width=80%}

<aside class="notes">
Clips are put in video and audio tracks within parallels
</aside>

## Video Still Frames

![Video Still Frames](images/timeline2.svg){width=80%}

<aside class="notes">
If the video track is shorter, it will be padded with still frames
</aside>

## Adding Gaps

![Adding Gaps](images/timeline3.svg){width=100%}

<aside class="notes">
- You can add explicit gaps in video and audio tracks
- These are also filled with still frames for video
</aside>

## Sequences

![Sequences](images/timeline4.svg){width=100%}

<aside class="notes">
- Parallels are put in sequences
- Each parallel is played until its end, then the next, and so on
- Multiple parallels can be used to synchronize clips
</aside>

## Timeline

![Timeline](images/timeline5.svg){width=100%}

<aside class="notes">
- The top level is the timeline
- The timeline contain sequences
- It's useful for organizing the parts of your screencast
</aside>

### Documentation

[The project website](https://owickstrom.github.io/komposition/) includes a
user guide, covering the concepts of the application, along with
recommendations on how to plan and record your screencasts to achieve the best
results and experience using Komposition.

![Komposition's user guide](/assets/writing-a-screencast-video-editor-in-haskell/documentation.png)

The landing page features a tutorial screencast, explaining how to import,
edit, and render a screencast. It's already a bit outdated, but I might get
around to making an updated version when doing the next release.

I can assure you, editing a screencast, that is about editing screencasts
using your screencast editor, in your screencast editor, is _quite the
mind-bender_. And I thought I had recursion down.

# Demo{background=#000000 background-video=images/demo.gif background-video-loop=true .dark}

# Implementation{background=images/cogs.jpg .dark}

## Striving for Purely Functional

- Pure functions and data structures in core domain
    - Timeline
    - Focus
    - Commands
    - Event handling
    - Key bindings
    - Parts of video classification
- Impure parts:
    - Audio and video import
    - Sentence classification
    - Preview frame rendering
    - Main application control flow

<aside class="notes">
- I'm striving to keep the core domain code pure
- "Functional core, imperative shell", Gary Bernhardt
- This includes: ...
- There are still impure parts, like: ...
</aside>

## GTK+

- Haskell bindings from `gi-gtk`
- Regular GTK+ was too painful
    - Imperative
    - Callback-oriented
    - Everything in IO, no explicit model
- Started `gi-gtk-declarative`
    - Declarative using data structures
    - VDOM-like diffing
    - Event handling based on pure functions and values
    - Custom widgets
- Imperative `gi-gtk` where needed

## Type-Indexed State Machines

- Using `motor` and `row-types` for typed state machines:

    ```haskell
    start
      :: Name n
      -> KeyMaps
      -> Actions m '[ n !+ State m WelcomeScreenMode] r ()
    ```

    ```haskell
    returnToTimeline
      :: ReturnsToTimeline mode
      => Name n
      -> TimelineModel
      -> Actions m '[ n := State m mode !--> State m TimelineMode] r ()
    ```
- Most complicated aspect of the codebase
- Currently being rewritten

<aside class="notes">
- I'm using a library I've been working on called "Motor" ...
- The most complicated aspect of the Komposition codebade
- Not sure if it's worth the complexity
    - But in combination with GADTs for state-specific events and commands ...
</aside>

## Singleton Pattern

```haskell
data Mode
  = WelcomeScreenMode
  | TimelineMode
  | LibraryMode
  | ImportMode

data SMode m where
  SWelcomeScreenMode :: SMode WelcomeScreenMode
  STimelineMode      :: SMode TimelineMode
  SLibraryMode       :: SMode LibraryMode
  SImportMode        :: SMode ImportMode
```

## Using Singletons

```haskell
data Command (mode :: Mode) where
  Cancel       :: Command mode
  Help         :: Command mode
  FocusCommand :: FocusCommand -> Command TimelineMode
  JumpFocus    :: Focus SequenceFocusType -> Command TimelineMode
  -- ...

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

## Automatic Scene Classification

- Creates a producer of frames

    ```haskell
    readVideoFile :: MonadIO m => FilePath -> Producer (Timed Frame) m ()
    ```
- Custom algorithm for classification

    ```haskell
    classifyMovement
        :: Monad m
        => Time -- ^ Minimum segment duration
        -> Producer (Timed RGB8Frame) m ()
        -> Producer (Classified (Timed RGB8Frame)) m ()

    classifyMovingScenes ::
         Monad m
      => Duration -- ^ Full length of video
      -> Producer (Classified (Timed RGB8Frame)) m ()
      -> Producer ProgressUpdate m [TimeSpan]
    ```

## Automatic Sentence Classification

- Currently using `sox`
    - Normalization
    - Noise gate
    - Auto-splitting by silence
- Creates segment audio files on disk (can't extract timespans)

## Rendering

- Flattening timeline
    - Conversion from hierarchical timeline to a flat representation
    - Pads gaps and empty parts with still frames
- Flat representation is converted to a FFmpeg command
    - Data types for FFmpeg CLI syntax
    - Common flags
    - Filter graph

## Preview

- Proxy media for performance
- Same FFmpeg backend as when rendering
    - Output is a streaming HTTP server
    - Not ideal, would like to use a named pipe or domain socket
- GStreamer widget
    - Consumes the HTTP stream
    - Embedded in the GTK+ user interface
- Unreliable
- Currently doesn't work on individual clips and gaps

# Testing{background=images/testing.jpg .dark}

## Color-Tinting Video Classifier

- Tints the original video with red/green based on classification
- Easier to test classifier on real recordings

![Komposition](images/color-tinting.gif)

## Property-Based Testing

- Timeline commands and movement
    - Generates sequences of commands
    - Applies all commands
    - Resulting focus should always be valid
- Video scene classification
    - Generates known test scenes
    - Translates to real frame data
    - Runs classifier, compares to known test scenes
- Flattening of hierchical timeline
- Roundtrip properties of FFmpeg format printers and parsers

## Example-Based Testing

- Commands
- Navigation
- FFmpeg syntax printing

# Used Packages

## haskell-gi

- Bindings for GTK+, GStreamer, and more
    - gi-gobject
    - gi-glib
    - gi-gst
    - gi-gtk
    - gi-gdk
    - gi-gdkpixbuf
    - gi-pango
- Extended with gi-gtk-declarative

## massiv & massiv-io

- Used in video classifier
- Parallel comparison of pixel arrays
- No-copy conversion from JuicyPixels frames to massiv arrays
- Lower-resolution proxy media helps with performance

## Pipes

- Streaming frame reader and writer around `ffmpeg-light`
- IO operations with streaming progress notifications

    ```haskell
    importVideoFileAutoSplit
      :: (MonadIO m, MonadSafe m)
      => VideoSettings
      -> FilePath
      -> FilePath
      -> Producer ProgressUpdate m [VideoAsset]
    ```
- `pipes-safe` for handling resources
- `pipes-parse` for `StateT`-based transformations

## Others

- protolude
- lens
- typed-process

# Summary

## Retrospective

- The best parts
    - Building a useful tool
    - Haskell and GHC
    - Keeping core domain pure
    - Testing with Hedgehog
- The problematic parts
    - Video and audio codecs, containers, streaming
    - Executing external programs
    - GTK+ in Haskell
    - Dependency managment (non-Haskell dependencies)

## Next Steps

- Features
    - More commands (yank, paste, join, ...)
    - Preview any timeline part
    - Adjust clips
- Improvements
    - Technical debt, refactoring
    - Content-addressed project files (reuse, avoiding collision)
    - Optimized FFmpeg rendering
    - Optimized diffing (gi-gtk-declarative)
- Packaging (Debian, macOS, Windows, nixpkgs)
- ... and much more

## Thank You!

- Komposition: [owickstrom.github.io/komposition/](https://owickstrom.github.io/komposition/)
- Slides: [owickstrom.github.io/writing-a-screencast-video-editor-in-haskell/](https://owickstrom.github.io/writing-a-screencast-video-editor-in-haskell/)
- Image credits:
    - [Yak by travelwayoflife - Flickr, CC BY-SA 2.0](https://commons.wikimedia.org/w/index.php?curid=22106967)
    - [Human evolution scheme by M. Garde - Self work (Original by: José-Manuel Benitos), CC BY-SA 3.0](https://commons.wikimedia.org/w/index.php?curid=2165296)
    - [Old Cogs by Emmanuel Huybrechts from Laval, Canada (Old Cogs) CC BY 2.0, via Wikimedia Commons](https://commons.wikimedia.org/wiki/File:Old_Cogs_(5084228263).jpg)
    - [Save the skateboard!](https://www.reddit.com/r/BetterEveryLoop/comments/6gsbs3/save_the_skateboard/)
- Thanks to [\@sassela](https://twitter.com/sassela) for great feedback!
