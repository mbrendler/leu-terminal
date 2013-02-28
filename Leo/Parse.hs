module Leo.Parse (
    xmlStringToParts
  , parseXmlToContent
  , queryXmlParts
  , xmlPartsToParts
  ) where

import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Util (docContent, contentElem, attrs)
import Text.XML.HaXml.Posn (posInNewCxt, Posn)
import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), AttValue(..))
import Text.XML.HaXml.Combinators (tag, (/>), txt, elm)

import Leo.Types (
    Part(..)
  , Direct(..)
  , Translation(..)
  , showContent
  )


xmlStringToParts :: String -> [Part Posn]
xmlStringToParts = xmlPartsToParts . queryXmlParts . parseXmlToContent

parseXmlToContent :: String -> Content Posn
parseXmlToContent xmlStr =
  docContent (posInNewCxt "" Nothing) (xmlParse "" xmlStr)

queryXmlParts :: Content i -> [Content i]
queryXmlParts = tag "xml" /> elm

xmlPartsToParts :: [Content i] -> [Part i]
xmlPartsToParts = concatMap xmlPartToPart

xmlPartToPart :: Content i -> [Part i]
xmlPartToPart (CElem (Elem (N "part") attributes sects) _) = let
    directFromAttr (AttValue [Left "1"]) = Direct
    directFromAttr _ = Indirect
    direct = maybe Indirect directFromAttr (lookup (N "direct") attributes)
    createPart (title, entries) = Part direct title entries
  in map (createPart . sectionData) $ sects
xmlPartToPart (CElem (Elem (N "similar") _ sides) _) = map partSimilar sides
xmlPartToPart (CElem (Elem (N "advMedia") _ _) _) = []
xmlPartToPart (CElem (Elem (N "search") _ _) _) = []
xmlPartToPart (CElem (Elem (N "forum") _ _) _) = []
xmlPartToPart (CElem (Elem (N "baseform") _ _) _) = []
xmlPartToPart (CElem (Elem (N "forumRef") _ _) _) = []
xmlPartToPart (CElem (Elem (N "servicedata") _ _) _) = []
xmlPartToPart x = [UNSUPPORTED_PART $ showContent x]

sectionData :: Content i -> (String, [Translation i])
sectionData (CElem (Elem (N "section") sattrs xmlEntries) _) = let
    titleFromAttr (Just (AttValue [Left title])) = title
    titleFromAttr _ = ""
  in (titleFromAttr $ lookup (N "sctTitle") sattrs,
      map entryToTranslation xmlEntries)
sectionData x = ("UNSUPORTED_SECTION: " ++ showContent x, [])

entryToTranslation :: Content i -> Translation i
entryToTranslation (CElem (Elem (N "entry") _ (side1:side2:[_info])) _) = let
    repr side = head $ tag "side" /> tag "repr" $ side
  in Translation (repr side1) (repr side2)
entryToTranslation x = UNSUPPORTED_TRANSLATION $ showContent x


partSimilar :: Content i -> Part i
partSimilar xmlSide = PartSimilar (getWordStrings xmlSide) (show lang)
  where
    getWordsContents = tag "side" /> tag "word" /> txt
    getWordStrings = map showContent . getWordsContents
    attributes = attrs $ contentElem xmlSide
    lang = snd $ head $ filter ((== (N "lang")) . fst) attributes
