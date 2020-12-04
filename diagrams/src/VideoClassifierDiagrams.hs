{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module VideoClassifierDiagrams where


import           Diagrams.Backend.Cairo
import           Diagrams.Prelude
import           TimelineDiagrams

movingClipBg   = sRGB24 168 176 119
movingClipLc   = darken 0.2 movingClipBg
stillClipBg   = sRGB24 176 119 168
stillClipLc   = darken 0.2 stillClipBg

data Motion = Still | Moving

data Segment = Segment Motion PartDuration (Maybe String)

renderSegments trackId segments' = zipWith renderPart ids segments' # hcat # alignL
 where
  ids = map (addToId trackId) [1 .. length segments']
  partLabel dur txt =
    (text txt # fontSizeL (textHeight * 0.6) # font "Linux Biolinum O")
    <> strutX (dur * 2) <> strutY 2
  renderPart id' (Segment motion dur mLabel) =
    maybe mempty (partLabel dur) mLabel <>
    rect (dur * 2) 2 # bg (segmentColor motion) # lc (segmentLineColor motion) # lwG 0.1 # named id'
  segmentColor Still  = movingClipBg
  segmentColor Moving = stillClipBg
  segmentLineColor Still  = movingClipLc
  segmentLineColor Moving = stillClipLc

renderPixelFrames trackId segments' = bgBox <> frames
 where
  bgBox = boundingRect frames # lc black # lwG 0.1
  frames = zipWith renderPart ids segments' # hcat # alignL
  ids = map (addToId trackId) [1 .. length segments']
  renderPart id' (Segment Moving dur _) =
    named id' . hcat $
    flip map (take (round (dur * 10)) (cycle [gray, white])) $ \color ->
      box' (1/10)
      # bg color
  renderPart id' (Segment Still dur _) =
    box' dur
    # bg black
    # named id'
  box' dur =
    rect (dur * 2) 2
    # lwG 0
