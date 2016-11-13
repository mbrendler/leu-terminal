module Leu.Parse (
  xmlStringToParts,
  xmlStringToContent,
  queryXmlParts,
  ) where

import Data.Maybe (fromMaybe, mapMaybe)

import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Util (docContent, contentElem, attrs)
import Text.XML.HaXml.Posn (posInNewCxt, Posn)
import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), AttValue(..))
import Text.XML.HaXml.Combinators (tag, (/>), txt, elm)

import Leu.Types (
  Part(..),
  Direct(..),
  Translation(..),
  showContent,
  showElement,
  )


xmlStringToParts :: String -> [Part Posn]
xmlStringToParts = concatMap xmlPartToPart . queryXmlParts . xmlStringToContent

xmlStringToContent :: String -> Content Posn
xmlStringToContent s = docContent (posInNewCxt "" Nothing) (xmlParse "" s)

queryXmlParts :: Content i -> [Content i]
queryXmlParts = tag "xml" /> elm

xmlPartToPart :: Content i -> [Part i]
xmlPartToPart = elementPartToPart . contentElem

elementPartToPart :: Element i -> [Part i]
elementPartToPart (Elem (N "part") attributes sects) = let
    directFromAttr (AttValue [Left "1"]) = Direct
    directFromAttr _ = Indirect
    direct = maybe Indirect directFromAttr (lookup (N "direct") attributes)
    createPart (title, entries) = Part direct title entries
  in map (createPart . sectionData) sects
elementPartToPart (Elem (N "sectionlist") _ sects) = let
    createPart (title, entries) = Part Direct title entries
  in map (createPart . sectionData) sects
elementPartToPart (Elem (N "similar") _ sides) = map partSimilar sides
elementPartToPart (Elem (N "advMedia") _ _) = []
elementPartToPart (Elem (N "search") _ _) = []
elementPartToPart (Elem (N "forum") _ _) = []
elementPartToPart (Elem (N "baseform") _ _) = []
elementPartToPart (Elem (N "forumRef") _ _) = []
elementPartToPart (Elem (N "servicedata") _ _) = []
elementPartToPart (Elem (N "ffsynlist") _ _) = []
elementPartToPart x = [UNSUPPORTED_PART $ showElement x]

sectionData :: Content i -> (String, [Translation i])
sectionData (CElem (Elem (N "section") sattrs xmlEntries) _) = let
    defaultTitle = AttValue [Left ""]
    sectionTitle = fromMaybe defaultTitle (lookup (N "sctTitle") sattrs)
    translations = mapMaybe entryToTranslation xmlEntries
  in (show sectionTitle, translations)
sectionData x = ("UNSUPORTED_SECTION: " ++ showContent x, [])

entryToTranslation :: Content i -> Maybe (Translation i)
entryToTranslation (CElem (Elem (N "entry") _ (side1:side2:_)) _) = let
    repr side = head $ tag "side" /> tag "repr" $ side
  in Just $ Translation (repr side1) (repr side2)
entryToTranslation (CElem (Elem (N "minprio") _ _) _) = Nothing
entryToTranslation x = Just $ UNSUPPORTED_TRANSLATION $ showContent x


partSimilar :: Content i -> Part i
partSimilar xmlSide = PartSimilar (getWordStrings xmlSide) (show lang)
  where
    getWordsContents = tag "side" /> tag "word" /> txt
    getWordStrings = map showContent . getWordsContents
    attributes = attrs $ contentElem xmlSide
    defaultLang = AttValue [Left ""]
    lang = fromMaybe defaultLang (lookup (N "lang") attributes) :: AttValue
