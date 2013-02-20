import System.Environment (getArgs)
import Codec.Binary.UTF8.String (decodeString)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody, Request)
import Network.HTTP.Headers (setHeaders, Header(Header), HeaderName(HdrCookie))

import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Reference(..), AttValue(..))
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Combinators (tag, (/>))
import Text.XML.HaXml.Util (docContent, tagTextContent)
import Text.XML.HaXml.Posn (posInNewCxt)
import Text.XML.HaXml.Html.Generate (htmlprint)

import TagColoring (clearSGR, tagToSGR)


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


xmlPartsToParts :: [Content i] -> [Part i]
xmlPartsToParts = concat . (map xmlPartToPart)

xmlPartToPart :: Content i -> [Part i]
xmlPartToPart (CElem (Elem (N "part") attrs sects) _) = let
    directFromAttr (AttValue [Left "1"]) = Direct
    directFromAttr _ = Indirect
    direct = maybe Indirect (directFromAttr) (lookup (N "direct") attrs)
    createPart (title, entries) = Part direct title entries
  in map (createPart . sectionData) sects
xmlPartToPart x = [UNSUPPORTED_PART $ tagTextContent x]


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


prettyPart :: Part i -> String
prettyPart (Part direct section entries) =
  show direct ++ ": " ++ section ++ "\n" ++ unlines (map prettyEntry entries)
prettyPart x = show x


prettyEntry :: Translation i -> String
prettyEntry (Translation a b) = reprToString a ++ " --- " ++ reprToString b
prettyEntry x = show x


contentsToString :: [Content i] -> String
contentsToString = concat . map reprToString

reprToString :: Content i -> String
reprToString (CElem (Elem (N tagName) _ subs) _) =
  tagToSGR tagName ++ contentsToString subs ++ clearSGR
reprToString (CElem (Elem (QN _ _) _ _) _) = "Not handled: QN"
reprToString (CString _ s _) = s
reprToString (CRef (RefEntity _) _) = ""  -- "RefEntity: " ++ n
reprToString (CRef (RefChar _) _) = ""  -- "RefChar: " ++ show n
reprToString (CMisc _ _) = "Misc"


buildLeoUrl :: String -> String
buildLeoUrl searchFor = "http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search=" ++ searchFor ++ "&searchLoc=0&resultOrder=basic&multiwordShowSingle=on"

-- Maybe I need to get the Cookie by a regular call to dict.leo.org at first!
addHeaders :: (Request a) -> (Request a)
addHeaders r = setHeaders r [Header HdrCookie "LEOABTEST=T; browser=webkit%3B5%3Bajax"]


main :: IO ()
main = do
  searchFor:_ <- getArgs
  query <- simpleHTTP . addHeaders . getRequest . buildLeoUrl $ searchFor
  queryResult <- getResponseBody query
  -- queryResult <- readFile "test_data/query_for_hello.xml"
  let document = xmlParse "" queryResult
  let content = docContent (posInNewCxt "" Nothing) document
  let xmlParts = tag "xml" /> tag "part" $ content
  let parts = xmlPartsToParts xmlParts
  -- TODO: filter parts by command line options
  putStrLn $ decodeString $ unlines $ map prettyPart parts
