import System.Environment (getArgs)
import Codec.Binary.UTF8.String (decodeString)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody, Request)
import Network.HTTP.Headers (setHeaders, Header(Header), HeaderName(HdrCookie))

import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Reference(..))

import TagColoring (clearSGR, tagToSGR)
import ParseLeo (
    xmlStringToParts
  , Part(Part)
  , Translation(Translation)
  )


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
  let parts = xmlStringToParts queryResult
  -- TODO: filter parts by command line options
  putStrLn $ decodeString $ unlines $ map prettyPart parts
