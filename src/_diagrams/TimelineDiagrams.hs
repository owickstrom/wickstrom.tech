{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module TimelineDiagrams where

import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty (..), toList)
import           Data.Typeable
import           Diagrams.Prelude

newtype Id = Id [Int]
  deriving (Typeable, Eq, Ord, Show)

instance IsName Id

addToId :: Id -> Int -> Id
addToId (Id ids) i = Id (ids <> [i])

prettyPrintId :: Id -> String
prettyPrintId (Id ids) = intercalate "." (map show ids)

newtype Timeline = Timeline (NonEmpty Sequence)

data FlatTimeline = FlatTimeline Track Track

newtype Sequence = Sequence (NonEmpty Parallel)

data Parallel = Parallel { videoTrack :: Track, audioTrack :: Track }

data Track = Track MediaType [Part]

data Implicitness = Explicit | Implicit
  deriving (Show)

type PartDuration = Double

data Part
  = Clip PartDuration (Maybe String)
  | Gap Implicitness PartDuration (Maybe String)

data MediaType = Video | Audio
  deriving (Show)

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

data RenderSettings = RenderSettings
  { containerLabels :: Bool
  , parallelArrows  :: Bool
  }

defaultRenderSettings = RenderSettings
  { containerLabels = False
  , parallelArrows  = False
  }

renderLabel lbl w =
  alignedText 0 0 lbl #fontSizeL textHeight # font "Linux Biolinum"
  <>
  alignBL (strutX w <> strutY defaultSpacing)

renderTrack (Track _ []) = strut 1
renderTrack (Track mt parts') = map renderPart parts' # hcat # alignL
 where
  partLabel dur txt =
    (text txt # fontSizeL (textHeight * 0.6) # font "Linux Biolinum")
    <> strutX (dur * 2) <> strutY 2
  renderPart (Clip dur mLabel) =
    maybe mempty (partLabel dur) mLabel <>
    rect (dur * 2) 2 # bg (partColor mt) # lc (partLineColor mt) # lwG 0.1
  renderPart (Gap impl dur mLabel) =
    maybe mempty (partLabel dur) mLabel <>
    rect (dur * 2) 2 # gapStyle impl # lwG 0.1
  partColor Video = videoClipBg
  partColor Audio = audioClipBg
  partLineColor Video = videoClipLc
  partLineColor Audio = audioClipLc
  gapStyle Implicit = bg implicitGapBg . lc gapLc . dashingG [0.3, 0.3] 1
  gapStyle Explicit = bg explicitGapBg . lc gapLc

leftAlignedLabelAbove settings lbl d =
  if containerLabels settings
    then alignL lbl === alignL d
    else alignL d

renderTracks settings t1 t2 =
  let box1    = renderTrack t1
      box2    = renderTrack t2
      children = if parallelArrows settings
        then [box1, trackArrow box1, box2, trackArrow box2]
        else [box1, box2]
  in  vsep 1 children # frame 1
 where
  trackArrow trackBox = arrowV'
    (with & arrowHead .~ tri & headLength .~ local 0.5)
    (width trackBox ^& 0)

renderParallel settings id' parallel =
  boxedTracks # leftAlignedLabelAbove settings lblText
 where
  tracks = renderTracks settings (videoTrack parallel) (audioTrack parallel)
  bgBox = boundingRect tracks # lc parallelLc # bg parallelBg # lwG 0.2
  boxedTracks = (tracks <> bgBox) # center # named id'
  lblText = renderLabel ("Parallel " <> prettyPrintId id') (width boxedTracks)

padLRB pad' dia = strutX pad' ||| (dia === strutY pad') ||| strutX pad'

connectArr = connectOutside' (with & arrowHead .~ tri & headLength .~ local 0.5)

addArrows ids = foldl (\f (i1, i2) -> connectArr i1 i2 . f) id (pairs ids)

renderSequence settings id' (Sequence parallels) =
  boxedParallels
  # addArrows ids
  # leftAlignedLabelAbove settings lblText
 where
  ids = map (addToId id') [1 .. length parallels]
  parallels' =
    parallels
      # toList
      # zipWith (renderParallel settings) ids
      # hsep defaultSpacing
      # padLRB defaultSpacing
  bgBox          = boundingRect parallels' # bg sequenceBg # lc darkgrey # lwG 0.2
  boxedParallels = (parallels' <> bgBox) # center # named id'
  lblText =
    renderLabel ("Sequence " <> prettyPrintId id') (width boxedParallels)

renderTimeline settings (Timeline sequences) = alignL lblText === alignL boxedSequences # addArrows ids
 where
  ids = [Id [n] | n <- [1..length sequences]]
  sequences' =
    sequences
      # toList
      # zipWith (renderSequence settings) ids
      # hsep defaultSpacing
      # padLRB defaultSpacing
  bgBox          = boundingRect sequences' # bg timelineBg # lc darkgrey # lwG 0.2
  boxedSequences = (sequences' <> bgBox) # center
  lblText        = renderLabel "Timeline" (width boxedSequences)

renderFlatTimeline settings (FlatTimeline t1 t2) =
  alignL lblText === alignL boxedTracks
 where
  bgBox       = boundingRect tracks # bg timelineBg # lc darkgrey # lwG 0.2
  boxedTracks = (tracks <> bgBox) # center
  tracks = renderTracks settings t1 t2 # center
  lblText = renderLabel "Flat Timeline" (width tracks)
