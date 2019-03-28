{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import           Data.List                      (intercalate)
import           Data.List.NonEmpty             (NonEmpty (..), toList)
import           Data.Typeable
-- import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

data Id = Id [Int]
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

textHeight = 1

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

renderTrack :: Track -> Diagram B
renderTrack (Track _ []) = strut 1
renderTrack (Track mt parts') = map renderPart parts' # hcat # alignL
 where
  renderPart (Clip dur) =
    rect (dur * 2) 3 # bg (partColor mt) # lc (partLineColor mt)
  renderPart (Gap impl dur) =
    rect (dur * 2) 3 # gapStyle impl
  partColor Video = videoClipBg
  partColor Audio = audioClipBg
  partLineColor Video = videoClipLc
  partLineColor Audio = audioClipLc
  gapStyle Implicit = bg implicitGapBg . lc gapLc . dashingN [0.005, 0.005] 1
  gapStyle Explicit = bg explicitGapBg . lc gapLc

data ParallelRenderMode = ParallelRenderSimple | ParallelRenderDetailed

renderParallel :: ParallelRenderMode -> Id -> Parallel -> Diagram B
renderParallel renderMode id' parallel = alignL lblText === alignL boxedTracks
 where
  vtBox  = renderTrack (videoTrack parallel)
  atBox  = renderTrack (audioTrack parallel)
  trackArrow trackBox = arrowV' (with & arrowHead .~ tri & headLength .~ local 0.5) (width trackBox ^& 0)
  tracks =
    let children =
          case renderMode of
            ParallelRenderSimple   -> [vtBox, atBox]
            ParallelRenderDetailed -> [vtBox, trackArrow vtBox, atBox, trackArrow atBox]
    in vsep 1 children # frame 1
  bgBox       =
    boundingRect tracks
    # lc parallelLc
    # bg parallelBg
  boxedTracks =
    (tracks <> bgBox)
    # center
    # named id'
  lblText = renderLabel ("Parallel " <> prettyPrintId id') (width boxedTracks)

padLRB pad' dia = strutX pad' ||| (dia === strutY pad') ||| strutX pad'

connectArr = connectOutside' (with & arrowHead .~ tri & headLength .~ local 0.5 & shaftStyle %~ dashingN [0.005, 0.005] 1)

addArrows ids = foldl (\f (i1, i2) -> connectArr i1 i2 . f) id (pairs ids)

renderSequence :: Id -> Sequence -> Diagram B
renderSequence id' (Sequence parallels) = alignL lblText === alignL boxedParallels # addArrows ids
 where
  ids =
    map (addToId id') [1..length parallels]
  parallels' =
    parallels
    # toList
    # zipWith (renderParallel ParallelRenderDetailed) ids
    # hsep (defaultSpacing * 1.5)
    # padLRB defaultSpacing
  bgBox =
    boundingRect parallels'
    # bg sequenceBg
    # lc darkgrey
  boxedParallels =
    (parallels' <> bgBox)
    # center
    # named id'
  lblText = renderLabel ("Sequence " <> prettyPrintId id') (width boxedParallels)

renderTimeline :: Timeline -> Diagram B
renderTimeline (Timeline sequences) = alignL lblText === alignL boxedSequences # addArrows ids
 where
  ids = [Id [n] | n <- [1..length sequences]]
  sequences' =
    sequences
      # toList
      # zipWith renderSequence ids
      # hsep (defaultSpacing * 1.5)
      # padLRB defaultSpacing
  bgBox          = boundingRect sequences' # bg timelineBg # lc darkgrey
  boxedSequences = (sequences' <> bgBox) # center
  lblText        = renderLabel "Timeline" (width boxedSequences)

main :: IO ()
main = multiMain
  [ ("timeline", renderTimeline timeline)
  , ("sequence", renderSequence (Id [1]) s1)
  , ("parallel", renderParallel ParallelRenderDetailed (Id [1]) p1)
  , ("test", test)
  ]
 where
  timeline = Timeline (s1 :| [s2])
  s1       = Sequence (p1 :| [p2])
  s2       = Sequence (p2 :| [p1])
  p1       = Parallel
    (Track Video [Clip 2, Gap Explicit 0.5, Clip 1, Gap Implicit 1.5])
    (Track Audio [Clip 4, Gap Explicit 1])
  p2 = Parallel (Track Video [Gap Explicit 1, Clip 4.5])
                (Track Audio [Clip 1.2, Gap Implicit 4.3])

test :: Diagram B
test =
  let x :: Diagram B
      x = arrowV (2 ^& 0) <> (rect 2 1 # bg lime)
      wrap d' = d' <> boundingRect d' # bg lightgrey # lc red
  in hsep 0 (replicate 4 (wrap x))
