---
layout: post
title: "Property-Based Testing in a Screencast Editor, Case Study 2: Video Scene Classification"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
excerpt: |
  TODO
---

{% diagram :width => 200, :caption => "A parallel with two video clips and one audio clip" %}

import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = renderParallel RenderSettings { labels = False, parallelArrows = True } (Id []) $ Parallel
  (Track Video [Clip 5, Clip 3])
  (Track Audio [Clip 8])
{% enddiagram %}

{% diagram :width => 750, :caption => "A timeline things in it" %}

import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = renderTimeline RenderSettings { labels = True, parallelArrows = False } timeline 
  where
    timeline = Timeline (s1 :| [s2])
    s1 = Sequence (p1 :| [p2])
    s2 = Sequence (p2 :| [p1])
    p1 = Parallel
      (Track Video [Clip 2, Gap Explicit 0.5, Clip 1, Gap Implicit 1.5])
      (Track Audio [Clip 4, Gap Explicit 1])
    p2 = Parallel
      (Track Video [Gap Explicit 1, Clip 4.5])
      (Track Audio [Clip 1.2, Gap Implicit 4.3])
{% enddiagram %}

## Credits

TODO...

## Footnotes

[^1]: ...
