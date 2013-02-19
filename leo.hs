import System.Environment (getArgs)
import System.Console.ANSI (setSGRCode,
                            SGR(SetColor),
                            ConsoleLayer(Foreground),
                            ColorIntensity(Dull, Vivid),
                            Color(Blue, Yellow))
import Codec.Binary.UTF8.String (decodeString)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.HTTP.Headers

import Text.XML.HaXml.Types (Content(..), Element(Elem), QName(N), Reference(..))
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Combinators (CFilter, tag, (/>))
import Text.XML.HaXml.Util (docContent)
import Text.XML.HaXml.Posn (posInNewCxt)


buildLeoUrl :: String -> String
buildLeoUrl searchFor = "http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search=" ++ searchFor ++ "&searchLoc=0&resultOrder=basic&multiwordShowSingle=on"
--                         http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search="                 "&searchLoc=0&resultOrder=basic&multiwordShowSingle=on

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

clearSGR = setSGRCode []
bSGR = setSGRCode [SetColor Foreground Vivid Blue]
smallSGR = setSGRCode [SetColor Foreground Dull Yellow]

sideRepr :: Content i -> String
sideRepr side = concat $ map reprToString repr
  where
    repr = tag "side" /> tag "repr" $ side
    contentsToString = concat . map reprToString
    reprToString (CElem (Elem (N tag) _ subs) _) 
      | tag == "repr" = contentsToString subs
      | tag == "b" = bSGR ++ contentsToString subs ++ clearSGR
      | tag == "small" = smallSGR ++ contentsToString subs ++ clearSGR
      | tag == "i" = contentsToString subs
      | otherwise = "CElem(" ++ tag ++ " " ++ contentsToString subs ++ ")"
    reprToString (CString _ s _) = s
    reprToString (CRef (RefEntity n) _) = ""  -- "RefEntity: " ++ n
    reprToString (CRef (RefChar n) _) = ""  -- "RefChar: " ++ show n
    reprToString (CMisc _ _) = "Misc"


-- Maybe I need to get the Cookie by a regular call to dict.leo.org at first!
--
-- All headers query.xml:
--Request URL:http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search=hallo&searchLoc=0&resultOrder=basic&multiwordShowSingle=on
--Request Method:GET
--Status Code:200 OK
--Request Headersview source
--Accept:text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
--Accept-Charset:ISO-8859-1,utf-8;q=0.7,*;q=0.3
--Accept-Encoding:gzip,deflate,sdch
--Accept-Language:de-DE,de;q=0.8,en-US;q=0.6,en;q=0.4
--Cache-Control:max-age=0
--Connection:keep-alive
--Cookie:LEOABTEST=T; browser=webkit%3B5%3Bajax
--Host:dict.leo.org
--User-Agent:Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17
--Query String Parametersview sourceview URL encoded
--tolerMode:nof
--lp:ende
--lang:de
--rmWords:off
--rmSearch:on
--search:hallo
--searchLoc:0
--
--
-- All headers dict.leo.org
--Request URL:http://dict.leo.org/
--Request Method:GET
--Status Code:304 Not Modified
--Request Headersview source
--Accept:text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
--Accept-Charset:ISO-8859-1,utf-8;q=0.7,*;q=0.3
--Accept-Encoding:gzip,deflate,sdch
--Accept-Language:de-DE,de;q=0.8,en-US;q=0.6,en;q=0.4
--Cache-Control:max-age=0
--Connection:keep-alive
--Cookie:LEOABTEST=T; browser=webkit%3B5%3Bajax
--Host:dict.leo.org
--If-Modified-Since:Tue, 19 Feb 2013 08:04:40 GMT
--User-Agent:Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17
--Response Headersview source
--Cache-Control:max-age=10
--Cache-Control:public, must-revalidate, proxy-revalidate
--Connection:keep-alive
--Date:Tue, 19 Feb 2013 08:11:49 GMT
--Expires:Tue, 19 Feb 2013 08:11:59 GMT
--Last-Modified:Tue, 19 Feb 2013 08:04:40 GMT
--Server:nginx
--
--addHeaders :: (Request a) -> (Request a)
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
