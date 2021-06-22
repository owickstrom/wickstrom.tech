module BrokenLinks where

import Quickstrom
import Data.Foldable (any)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))

readyWhen = "body"

actions :: Actions
actions = [
  click "a[href^='/']"
]

patterns = map Pattern [ "Access Denied" ]

bodyText = maybe "" _.textContent (queryOne "body" { textContent })

hasErrorCode = any (\p -> contains p bodyText) patterns

proposition = always (not hasErrorCode)