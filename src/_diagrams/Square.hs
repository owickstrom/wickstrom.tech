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

textHeight = 0.3

lineHeight = textHeight * 1.5

defaultSpacing = lineHeight * 2

renderLabel lbl w =
  strutY (defaultSpacing - lineHeight)
  ===
  (text' <> alignBL (strutX w <> strutY lineHeight))

  where
    text' =
      alignedText 0 0 lbl
      # fontSizeL textHeight
      # font "Linux Biolinum"

renderTrack :: Track -> Diagram B
renderTrack (Track _ []) = strut 1
renderTrack (Track mt parts) = map renderPart parts # hcat # alignL
 where
  renderPart (Clip dur)     = rect dur 1 # fc (partColor mt) # lc black # lw 1
  renderPart (Gap impl dur) = rect dur 1 # fc darkgrey # lineStyle impl
  partColor Video = springgreen
  partColor Audio = gold
  lineStyle Implicit = dashingN [0.02,0.02] 0 # lc black # lw 1
  lineStyle Explicit = lc black # lw 1

renderParallel :: Id -> Parallel -> Diagram B
renderParallel id' parallel = alignL lblText === alignL boxedTracks
 where
  vtBox  = renderTrack (videoTrack parallel)
  atBox  = renderTrack (audioTrack parallel)
  tracks =
    vsep 0.2 [sized (mkHeight 1) vtBox, sized (mkHeight 1) atBox]
    # frame 0.2
  bgBox       = boundingRect tracks # fc lightgrey # lc black # lw 1
  boxedTracks =
    (tracks <> bgBox)
    # center
    # named id'
  lblText = renderLabel ("Parallel " <> prettyPrintId id') (width boxedTracks)

padLRB pad' dia= strutX pad' ||| (dia === strutY pad') ||| strutX pad'

renderSequence :: Id -> Sequence -> Diagram B
renderSequence id' (Sequence parallels) = alignL lblText === alignL boxedParallels
 where
  parallels' =
    parallels
    # toList
    # zipWith (renderParallel . addToId id') [1..]
    # hsep 1.2
    # padLRB defaultSpacing
  bgBox =
    boundingRect parallels'
    # lc darkgrey
    # lw 1
  boxedParallels =
    (parallels' <> bgBox)
    # center
    # named id'
  lblText = renderLabel ("Sequence " <> prettyPrintId id') (width boxedParallels)

renderTimeline :: Timeline -> Diagram B
renderTimeline (Timeline sequences) = padLRB defaultSpacing (alignL lblText === alignL boxedSequences)
 where
  sequences' =
    sequences
    # toList
    # zipWith (renderSequence . Id . pure ) [1..]
    # hsep 1.2
    # padLRB defaultSpacing
  bgBox =
    boundingRect sequences'
    # lc darkgrey
    # lw 1
  boxedSequences =
    (sequences' <> bgBox)
    # center
  lblText = renderLabel "Timeline" (width boxedSequences)

timeline = renderTimeline
  (Timeline (s1 :| [s1]))
  where
    s1 =
      Sequence
       (  Parallel
         (Track Video [Clip 2, Gap Explicit 0.5, Clip 1, Gap Implicit 1.5])
         (Track Audio [Clip 4, Gap Explicit 1])

         :| [ Parallel (Track Video [Gap Explicit 1, Clip 4.5])
              (Track Audio [Clip 1.2, Gap Implicit 4.3])
            ]
       )


timelineWithArrows =
  -- TODO: Automate this
  timeline
  # connectOutside (Id [1]) (Id [2])
  # connectOutside (Id [1, 1]) (Id [1, 2])
  # connectOutside (Id [2, 1]) (Id [2, 2])

main :: IO ()
main = multiMain [("timeline", timelineWithArrows)]
