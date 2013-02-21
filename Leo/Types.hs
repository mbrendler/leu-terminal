module Leo.Types (
    Part(..)
  , Translation(..)
  , Direct(..)
  ) where

import Text.XML.HaXml.Types (Content)
import Text.XML.HaXml.Html.Generate (htmlprint)


instance Show (Content i) where
  show x = show $ htmlprint [x]


data Translation i = Translation (Content i) (Content i)
                   | UNSUPPORTED_TRANSLATION String
                   deriving (Show)

data Direct = Direct | Indirect deriving (Show)

type Title = String

data Part i = Part Direct Title [Translation i]
            | UNSUPPORTED_PART String
            deriving (Show)
