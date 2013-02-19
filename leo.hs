import System.Environment (getArgs)
import Codec.Binary.UTF8.String (decodeString)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody, Request)
import Network.HTTP.Headers (setHeaders, Header(Header), HeaderName(HdrCookie))

import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Reference(..))
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Combinators (CFilter, tag, (/>))
import Text.XML.HaXml.Util (docContent)
import Text.XML.HaXml.Posn (posInNewCxt)

import TagColoring (clearSGR, tagToSGR)


sectionsFilter :: CFilter i
sectionsFilter = tag "xml" /> tag "part" /> tag "section"

prettySections :: [Content i] -> String
prettySections = showEntries . concat . (map parseSection)
  where
    showEntries = foldl (\m entry -> m ++ "\n" ++ showEntry entry) ""
    showEntry (x, y) = x ++ " --- " ++ y

parseSection :: Content i -> [(String, String)]
parseSection content = map entryToTuple $ tag "section" /> tag "entry" $ content

entryToTuple :: Content i -> (String, String)
entryToTuple entry = (sideRepr side1, sideRepr side2)
  where (side1:side2:[]) = tag "entry" /> tag "side" $ entry

sideRepr :: Content i -> String
sideRepr side = concat $ map reprToString repr
  where repr = tag "side" /> tag "repr" $ side

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
  let sections = sectionsFilter content
  putStrLn . decodeString . prettySections $ sections
