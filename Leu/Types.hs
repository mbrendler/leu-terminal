module Leu.Types (
  Part(..),
  Translation(..),
  Direct(..),
  showContent,
  showElement,
  LanguageMapping(..),
  allLanguageMappings,
  readLang,
  Terminal(..),
  buildTerminal,
  pretty,
  ) where

import System.IO (stdout)
import System.Console.ANSI (hSupportsANSI)
import System.Console.Terminal.Size (size, Window(..))
import Data.Maybe (fromMaybe)

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


data LanguageMapping = EnDe | FrDe | EsDe | ItDe | ChDe | RuDe | PtDe | PlDe
  deriving (Show, Enum, Bounded)

class Pretty a where
  pretty :: a -> String

instance Pretty LanguageMapping where
  pretty EnDe = "EnDe: English    - German"
  pretty FrDe = "FrDe: French     - German"
  pretty EsDe = "EsDe: Spanish    - German"
  pretty ItDe = "ItDe: Italian    - German"
  pretty ChDe = "ChDe: Chinese    - German"
  pretty RuDe = "RuDe: Russian    - German"
  pretty PtDe = "PtDe: Portuguese - German"
  pretty PlDe = "PlDe: Polish     - German"

allLanguageMappings :: [LanguageMapping]
allLanguageMappings = [minBound ..]

readLang :: String -> LanguageMapping
readLang x = if null langMaps then EnDe else head langMaps
  where
    langMaps = [l | l <- allLanguageMappings, toLowerCase x == toLowerCase (show l)]


type Width = Int
type ColorSupport = Bool

data Terminal = Terminal Width ColorSupport
  deriving Show

buildTerminal :: IO Terminal
buildTerminal = do
  maybeWindow <- size
  let window = fromMaybe (Window {height=25, width=80}) maybeWindow
  colorSupport <- hSupportsANSI stdout
  return $ Terminal (width window) colorSupport
