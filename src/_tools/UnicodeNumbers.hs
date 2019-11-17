{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.Function         ((&))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Read        as Text
import           Text.Pandoc.JSON
import           Text.Regex.PCRE.Heavy

import Debug.Trace

main = toJSONFilter unicodeNumbersBlock

validFormat fmt = fmt `elem` ["html5", "latex"]

unicodeNumbersBlock :: Maybe Format -> Block -> Block
unicodeNumbersBlock (Just format) (CodeBlock attrs@(_, classes, _) code)
  | validFormat format && "numbers" `elem` classes =
  let replacer = if "haskell" `elem` classes then replaceWithUnicode else replaceWithNothing
  in code
      & Text.pack
      & Text.lines
      & map (sub [re|\-\-\ (\d+)|] replacer)
      & Text.unlines
      & Text.unpack
      & CodeBlock attrs
unicodeNumbersBlock (Just format) (Para inlines)
  | validFormat format = Para (map unicodeNumbersInline inlines)
unicodeNumbersBlock (Just format) (Plain inlines)
  | validFormat format = Plain (map unicodeNumbersInline inlines)
unicodeNumbersBlock _ x = x

unicodeNumbersInline :: Inline -> Inline
unicodeNumbersInline (Str str) =
    str
      & Text.pack
      & sub [re|\((\d+)\)|] replaceWithUnicode
      & Text.unpack
      & Str
unicodeNumbersInline x = x

replaceWithUnicode :: [Text] -> Text
replaceWithUnicode matches =
  let replace' g = case Text.decimal g of
        Right (c, "")
          | c <= 10 -> Text.singleton (toEnum (10101 + c))
          | c > 10 && c <= 20 -> Text.singleton (toEnum (9450 + (c `mod` 10)))
        Right _ -> g
        Left{} -> "???"
  in Text.concat (map replace' matches)

replaceWithNothing :: [Text] -> Text
replaceWithNothing = const mempty

