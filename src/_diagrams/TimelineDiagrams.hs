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

newtype Sequence = Sequence (NonEmpty Parallel)

data Parallel = Parallel { videoTrack :: Track, audioTrack :: Track }

data Track = Track MediaType [Part]

data Implicitness = Explicit | Implicit

type PartDuration = Double

data Part = Clip PartDuration | Gap Implicitness PartDuration

data MediaType = Video | Audio

pairs xs = zip xs (tail xs)

textHeight = 1.75

defaultSpacing = 2.5

timelineBg    = sRGB24 250 250 250
sequenceBg    = sRGB24 240 240 240
parallelBg    = white
parallelLc    = sRGB24 100 100 100
videoClipBg   = sRGB24 68  208 98
videoClipLc   = darken 0.2 videoClipBg
audioClipBg   = sRGB24 252 214 22
audioClipLc   = darken 0.2 audioClipBg
explicitGapBg = sRGB24 150 150 150
implicitGapBg = sRGB24 200 200 200
gapLc = darken 0.2 explicitGapBg

renderLabel lbl w =
  alignedText 0 0 lbl #fontSizeL textHeight # font "Linux Biolinum"
  <>
  alignBL (strutX w <> strutY defaultSpacing)

renderTrack (Track _ []) = strut 1
renderTrack (Track mt parts') = map renderPart parts' # hcat # alignL
 where
  renderPart (Clip dur) =
    rect (dur * 2) 3 # bg (partColor mt) # lc (partLineColor mt) # lwL 0.1
  renderPart (Gap impl dur) =
    rect (dur * 2) 3 # gapStyle impl # lwL 0.1
  partColor Video = videoClipBg
  partColor Audio = audioClipBg
  partLineColor Video = videoClipLc
  partLineColor Audio = audioClipLc
  gapStyle Implicit = bg implicitGapBg . lc gapLc . dashingN [0.005, 0.005] 1
  gapStyle Explicit = bg explicitGapBg . lc gapLc

data RenderSettings = RenderSettings
  { labels         :: Bool
  , parallelArrows :: Bool
  }

leftAlignedLabelAbove settings lbl d =
  if labels settings
    then alignL lbl === alignL d
    else alignL d

renderParallel settings id' parallel =
  boxedTracks # leftAlignedLabelAbove settings lblText
 where
  vtBox = renderTrack (videoTrack parallel)
  atBox = renderTrack (audioTrack parallel)
  trackArrow trackBox = arrowV'
    (with & arrowHead .~ tri & headLength .~ local 0.5)
    (width trackBox ^& 0)
  tracks =
    let children = if parallelArrows settings
          then [vtBox, trackArrow vtBox, atBox, trackArrow atBox]
          else [vtBox, atBox]
    in  vsep 1 children # frame 1
  bgBox = boundingRect tracks # lc parallelLc # bg parallelBg
  boxedTracks = (tracks <> bgBox) # center # named id'
  lblText = renderLabel ("Parallel " <> prettyPrintId id') (width boxedTracks)

padLRB pad' dia = strutX pad' ||| (dia === strutY pad') ||| strutX pad'

connectArr = connectOutside' (with & arrowHead .~ tri & headLength .~ local 0.5 & shaftStyle %~ dashingN [0.005, 0.005] 1)

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
      # hsep (defaultSpacing * 1.5)
      # padLRB defaultSpacing
  bgBox          = boundingRect parallels' # bg sequenceBg # lc darkgrey
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
      # hsep (defaultSpacing * 1.5)
      # padLRB defaultSpacing
  bgBox          = boundingRect sequences' # bg timelineBg # lc darkgrey
  boxedSequences = (sequences' <> bgBox) # center
  lblText        = renderLabel "Timeline" (width boxedSequences)
