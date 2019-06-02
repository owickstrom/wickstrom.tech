---
layout: post
title: "Property-Based Testing in a Screencast Editor, Case Study 3: Integration Testing"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
excerpt: | TODO
---

In the last post we looked at how Komposition automatically classifies
moving and still segments in imported video media, how I went from
testing by eye and with examples to testing with properties, and what
kinds of errors I found in the process. If you haven't read it, or its
preceding posts, I encourage you to check them out first:

1. [Introduction](/programming/2019/03/02/property-based-testing-in-a-screencast-editor-introduction.html)
1. [Timeline Flattening](/programming/2019/03/24/property-based-testing-in-a-screencast-editor-case-study-1.html)
1. [Video Scene Classification](/programming/2019/04/17/property-based-testing-in-a-screencast-editor-case-study-2.html)

This is the final case study in the "Property-Based Testing in a
Screencast Editor" series. It covers property-based integration
testing techniques and their value in aggressive refactoring work
within the Komposition video editor.

## A History of Two Stacks

In Komposition, a project's state is represented using an in-memory data
structure. It contains the hierarchical timeline, the focus, import and
render settings, file paths to storage locations on disk, and more. To let
users navigate backwards and forwards in their history of project edits, for
example when they have made a mistake, Komposition features _undo_ and _redo_
commands.

The undo/redo history was previously implemented as a data structure
recording project states, compromised of:

* a _current state_ variable two stacks, holding _previous states_ and
* _possible future states_, respectively

It had the following behaviour:

* Performing an _undoable_ action would:
  - push the previous state onto the undo stack
  - perform the action and replace the current state
  - discard redo stack
* Undoing an action would:
  - pop the undo stack and use that state as the current state
  - push the previous state onto the redo stack
* Redoing an action would:
  - pop the redo stack and use that state as the current state
  - push the previous state onto the undo stack

Note that not all user actions in Komposition are undoable/redoable. Actions
like navigating the focus or zooming are not recorded in the history.

### Dealing With Performance Problems

While the "two stacks of states" algorithm was easy to understand and
implement, it failed to meet my non-functional requirements. A screencast
project compromised of hundreds or thousands of small edits would consume
gigabytes of disk space when stored, take tens of seconds to load from disk,
and consume many gigabytes of RAM when in-memory.

Now, you might think that my implementation was incredibly naive, and that
the performance problems would be fixed with careful profiling and
optimization. And you'd probably be right! I did consider going down that
route, optimizing the code, time-windowing edits to compact history on the
fly, and capping the history at some fixed size. Those would all be
interesting pursuits, but in the end I decided to try something else.

## Refactoring with Property-Based Integration Tests

Instead of optimizing the current stack-based implementation, I decided to
implement the undo/redo history in terms of _involutive_ actions. In this
model, actions not only modify the project state, they also return another
action that effectively _reverses_ the effects of the original action.
Instead of recording a new project state data structure for each edit, the
history only records descriptions of the actions themselves.

I realized early that introducing the new undo/redo history implementation in
Komposition was not going to be a small task. It would touch the majority of
command implementation code, large parts of the main application logic, and
the project binary serialization format. What it wouldn't affect, though, was
the module describing user commands in abstract.

To provide a safety net for the refactoring, I decided to cover the undo/redo
functionality with tests. As the user commands would stay the same throughout
the modifications, I chose to test at that level, which can be characterized
as integration-level testing.

With close to twenty types of user commands, combined with a complex
hierarchical timeline and navigation behaviour, the combinatory explosion of
possible states was daunting. Relying on example-based tests to safeguard my
work would likely not cut it. While property-based tests couldn't cover the
entire state space either, I was confident they would improve my chances of
finding actual bugs.

### Testing Undo

* Generate an initial state
* Generate a sequence of undoable commands
* Run all commands
* Run undo command for each original command
* Assert that we end up at the initial state

## Actions are Undoable

```{.haskell}
hprop_undo_actions_are_undoable = property $ do

  -- Generate initial timeline and focus
  timelineAndFocus <- forAllWith showTimelineAndFocus $
    Gen.timelineWithFocus (Range.linear 0 10) Gen.parallel

  -- Generate initial application state
  initialState <- forAll (initializeState timelineAndFocus)

  -- Generate a sequence of undoable/redoable commands
  events <- forAll $
    Gen.list (Range.exponential 1 100) genUndoableTimelineEvent

  ...
```

## Actions are Undoable (cont.)

```{.haskell}
  ...

  -- We begin by running 'events' on the original state
  beforeUndos <- runTimelineStubbedWithExit events initialState

  -- Then we run as many undo commands as undoable commands
  afterUndos <- runTimelineStubbedWithExit (undoEvent <$ events) beforeUndos

  -- That should result in a timeline equal to the one we at the
  -- beginning
  timelineToTree (initialState ^. currentTimeline)
    === timelineToTree (afterUndos ^. currentTimeline)
```

## Testing Redo

* Generate an initial state
* Generate a sequence of undoable/redoable commands
* Run all commands
* Run undo _and redo_ commands for each original command
* Assert that we end up at the state before running undos

## Actions are Redoable

```{.haskell}
hprop_undo_actions_are_redoable = property $ do

  -- Generate the initial timeline and focus
  timelineAndFocus <- forAllWith showTimelineAndFocus $
    Gen.timelineWithFocus (Range.linear 0 10) Gen.parallel

  -- Generate the initial application state
  initialState <- forAll (initializeState timelineAndFocus)

  -- Generate a sequence of undoable/redoable commands
  events <- forAll $
    Gen.list (Range.exponential 1 100) genUndoableTimelineEvent
```

## Actions are Redoable (cont.)

```{.haskell}
  -- We begin by running 'events' on the original state
  beforeUndos <- runTimelineStubbedWithExit events initialState

  -- Then we undo and redo all of them
  afterRedos  <-
    runTimelineStubbedWithExit (undoEvent <$ events) beforeUndos
    >>= runTimelineStubbedWithExit (redoEvent <$ events)

  -- That should result in a timeline equal to the one we had before
  -- starting the undos
  timelineToTree (beforeUndos ^. currentTimeline)
    === timelineToTree (afterRedos ^. currentTimeline)
```


## Undo/Redo Test Summary

* These tests made the refactoring possible
* Founds _many_ interim bugs
  - Off-by-one index
  - Inconsistent focus
  - Non-invertible actions
* After the tests passed: ran the GUI, it worked

## Related Tests

* Focus and Timeline Consistency
    - The _focus_ is a data structure that "points" to a part of the
    timeline
    - The timeline and focus must at all points be consistent
    - Approach:
      - Generate a random initial state
      - Generate a random sequence of user commands
      - Check consistency after each command
      - Run all commands until termination

# Wrapping Up

## Summary

* Property-based testing is not only for pure functions!
  - Effectful actions
  - Integration tests
* Process (iterative)
    - Think about the specification first
    - Think about how generators and tests should work
    - Get minimal examples of failures, fix the implementation
* Using them in Komposition:
  - Made refactoring and evolving large parts of the system tractable
    and safer
  - Found existing errors in my thinking, my tests, my implementation
  - It's been a joy
