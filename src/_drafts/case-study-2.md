---
layout: post
title: "Property-Based Testing in a Screencast Editor, Case Study 2: Video Scene Classification"
author: Oskar Wickström
categories: programming
tags: ["property", "testing", "quality", "correctness", "haskell"]
excerpt: |
  TODO
---

{% diagram :width => 350, :caption => "Clips and gaps are placed in video and audio tracks" %}
import           Diagrams.Direction (dir)
import           TimelineDiagrams
import           Data.List.NonEmpty (NonEmpty (..))

dia :: Diagram B
dia = 
  Parallel
    (Track Video [Clip 4 (Just "Video Clip 1"), Gap Explicit 2 (Just "Gap"), Gap Explicit 3 (Just "Gap")])
    (Track Audio [Clip 6 (Just "Audio Clip 1")])
  # renderParallel defaultRenderSettings { parallelArrows = False } (Id [])
  # connectParts arrowRot (Id [1, 2]) (Id [1, 1])
  # connectParts arrowRot (Id [1, 3]) (Id [1, 1])
  where
    arrowRot = 1/10 @@ turn
{% enddiagram %}

## Credits

TODO...

## Footnotes

[^1]: ...
