module Leo.Types (
    Part(..)
  , Translation(..)
  , Direct(..)
  , showContent
  ) where

import Text.XML.HaXml.Types (Content)
import Text.XML.HaXml.Html.Generate (htmlprint)


showContent :: Content i -> String
showContent = show . htmlprint . (:[])


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
            | PartSimilar Word Language
            | UNSUPPORTED_PART String
            deriving (Show)
