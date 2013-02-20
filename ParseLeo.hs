module ParseLeo (
    xmlStringToParts
  , parseXmlToContent
  , queryXmlParts
  , xmlPartsToParts
  , Part(..)
  , Translation(..)
  ) where

import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Util (docContent)
import Text.XML.HaXml.Posn (posInNewCxt, Posn)
import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), AttValue(..))
import Text.XML.HaXml.Combinators (tag, (/>))
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


xmlStringToParts :: String -> [Part Posn]
xmlStringToParts = xmlPartsToParts . queryXmlParts . parseXmlToContent

parseXmlToContent :: String -> Content Posn
parseXmlToContent xmlStr =
  docContent (posInNewCxt "" Nothing) (xmlParse "" xmlStr)

queryXmlParts :: Content i -> [Content i]
queryXmlParts = tag "xml" /> tag "part"

xmlPartsToParts :: [Content i] -> [Part i]
xmlPartsToParts = concatMap xmlPartToPart

xmlPartToPart :: Content i -> [Part i]
xmlPartToPart (CElem (Elem (N "part") attrs sects) _) = let
    directFromAttr (AttValue [Left "1"]) = Direct
    directFromAttr _ = Indirect
    direct = maybe Indirect directFromAttr (lookup (N "direct") attrs)
    createPart (title, entries) = Part direct title entries
  in map (createPart . sectionData) sects
xmlPartToPart x = [UNSUPPORTED_PART $ show x]

sectionData :: Content i -> (String, [Translation i])
sectionData (CElem (Elem (N "section") sattrs xmlEntries) _) = let
    titleFromAttr (Just (AttValue [Left title])) = title
    titleFromAttr _ = ""
  in (titleFromAttr $ lookup (N "sctTitle") sattrs,
      map entryToTranslation xmlEntries)
sectionData x = ("UNSUPORTED_SECTION: " ++ show x, [])

entryToTranslation :: Content i -> Translation i
entryToTranslation (CElem (Elem (N "entry") _ (side1:side2:[_info])) _) = let
    repr side = head $ tag "side" /> tag "repr" $ side
  in Translation (repr side1) (repr side2)
entryToTranslation x = UNSUPPORTED_TRANSLATION $ show [x]
