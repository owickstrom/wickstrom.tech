{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module UndoRedoDiagrams where

import           Data.Typeable
import           Diagrams.Prelude

newtype Stack a = Stack [a]

data History a = History
  { previous :: Stack a, current :: a, future :: Stack a }

pairs xs = zip xs (tail xs)

textHeight = 1.2

defaultSpacing = 2.5

timelineBg    = sRGB24 250 250 250
sequenceBg    = sRGB24 240 240 240
parallelBg    = white
parallelLc    = darkgrey
videoClipBg   = sRGB24 168 176 119
videoClipLc   = darken 0.2 videoClipBg
audioClipBg   = sRGB24 176 119 168
audioClipLc   = darken 0.2 audioClipBg
explicitGapBg = sRGB24 220 220 220
implicitGapBg = white
gapLc = darken 0.2 explicitGapBg

renderBox bgColor suffix x =
  valueLabel (pure x) <> rect 1 1 # bg bgColor # lc black # named (x : suffix)
 where
  valueLabel txt =
    (text txt # fontSizeG 0.2 # font "Linux Biolinum") <> strutX 1 <> strutY 1

topLabelled txt d = alignL lbl === alignL d
  where lbl = text txt # fontSizeG 0.2 # font "Linux Biolinum" <> strutX 1 <> strutY 0.5

renderStack suffix (Stack xs) =
  map (renderBox (sRGB24 220 220 220) suffix) xs # vcat # alignL

renderHistory suffix History {..} =
  topLabelled "Undo" (renderStack suffix previous)
    ||| strutX 0.25
    ||| topLabelled "Current" (renderBox (sRGB24 176 119 168) suffix current)
    ||| strutX 0.25
    ||| topLabelled "Redo" (renderStack suffix future)

connectBoxes =
  connect' (with & gaps .~ 20
                 & shaftStyle %~ (dashing [10] 1 . lc gray))
