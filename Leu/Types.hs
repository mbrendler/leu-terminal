module Leu.Types (
    Part(..)
  , Translation(..)
  , Direct(..)
  , showContent
  , showElement
  ) where

import Text.XML.HaXml.Types (Content, Element)
import Text.XML.HaXml.Html.Pretty (content, element)


showContent :: Content i -> String
showContent = show . content

showElement :: Element i -> String
showElement = show . element


data Translation i = Translation (Content i) (Content i)
                   | UNSUPPORTED_TRANSLATION String

instance Show (Translation i) where
  show (Translation x y) = "Translation " ++
                           showContent x ++ " " ++
                           showContent y
  show (UNSUPPORTED_TRANSLATION x) = "UNSUPPORTED_TRANSLATION " ++ x


data Direct = Direct | Indirect deriving (Show)

type Title = String
type Word = String
type Language = String

data Part i = Part Direct Title [Translation i]
            | PartSimilar [Word] Language
            | UNSUPPORTED_PART String
            deriving (Show)
