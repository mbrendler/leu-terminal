import System.Environment (getArgs)
import System.Console.ANSI (setSGRCode,
                            SGR(SetColor),
                            ConsoleLayer(Foreground),
                            ColorIntensity(Dull, Vivid),
                            Color(Blue, Yellow))
import Codec.Binary.UTF8.String (decodeString)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import Text.XML.HaXml.Types (Content(..), Element(Elem), QName(N), Reference(..))
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Combinators (CFilter, tag, (/>))
import Text.XML.HaXml.Util (docContent)
import Text.XML.HaXml.Posn (posInNewCxt)


buildLeoUrl :: String -> String
buildLeoUrl searchFor = "http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search=" ++ searchFor ++ "&searchLoc=0&resultOrder=basic&multiwordShowSingle=on"

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

main :: IO ()
main = do
  searchFor:_ <- getArgs
  query <- simpleHTTP . getRequest . buildLeoUrl $ searchFor
  queryResult <- getResponseBody query
  -- queryResult <- readFile "test_data/query_for_hello.xml"
  let document = xmlParse "" queryResult
  let content = docContent (posInNewCxt "" Nothing) document
  let sections = sectionsFilter content
  putStrLn . decodeString . prettySections $ sections
