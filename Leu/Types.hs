module Leu.Types (
  Part(..),
  Translation(..),
  Direct(..),
  showContent,
  showElement,
  LanguageMapping(..),
  lDescription,
  allLanguageMappings,
  readLang,
  Terminal(..),
  buildTerminal,
  ) where

import System.IO (stdout)
import System.Console.ANSI (hSupportsANSI)
import TermSize (getTermSize)

import Text.XML.HaXml.Types (Content, Element)
import Text.XML.HaXml.Html.Pretty (content, element)

import Leu.Utils (toLowerCase)


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
            | PartSimilar [Leu.Types.Word] Language
            | UNSUPPORTED_PART String
            deriving (Show)


data LanguageMapping = EnDe
                     | FrDe
                     | EsDe
                     | ItDe
                     | ChDe
                     | RuDe
                     | PtDe
                     | PlDe
                     deriving (Show, Enum, Bounded)

lDescription :: LanguageMapping -> String
lDescription EnDe = "English    - German"
lDescription FrDe = "French     - German"
lDescription EsDe = "Spanish    - German"
lDescription ItDe = "Italian    - German"
lDescription ChDe = "Chinese    - German"
lDescription RuDe = "Russian    - German"
lDescription PtDe = "Portuguese - German"
lDescription PlDe = "Polish     - German"

allLanguageMappings :: [LanguageMapping]
allLanguageMappings = [minBound ..]

readLang :: String -> LanguageMapping
readLang x = if null langMaps then EnDe else head langMaps
  where
    langMaps = [l | l <- allLanguageMappings, toLowerCase x == toLowerCase (show l)]


type Width = Int
type ColorSupport = Bool

data Terminal = Terminal Width ColorSupport

buildTerminal :: IO Terminal
buildTerminal = do
  (_, width) <- getTermSize (Just (25, 80))
  colorSupport <- hSupportsANSI stdout
  return $ Terminal width colorSupport
