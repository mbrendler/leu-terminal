import System.Environment (getArgs)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.XML.HaXml.Types (Content)
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Combinators (CFilter, tag, (/>), elm, txt, o)
import Text.XML.HaXml.Util (docContent)
import Text.XML.HaXml.Posn (posInNewCxt)
import Text.XML.HaXml.Html.Generate (htmlprint)


buildLeoUrl :: String -> String
buildLeoUrl searchFor = "http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search=" ++ searchFor ++ "&searchLoc=0&resultOrder=basic&multiwordShowSingle=on"

sectionsFilter :: CFilter i
sectionsFilter = tag "xml" /> tag "part" /> tag "section"

prettySections :: [Content i] -> String
prettySections = showEntries . concat . (map parseSection)
  where
    showEntries = foldl (\m entry -> m ++ "\n" ++ showEntry entry) ""
    showEntry (x, y) = show x ++ " : " ++ show y

parseSection :: Content i -> [(String, String)]
parseSection content = map entryToTuple $ tag "section" /> tag "entry" $ content

entryToTuple :: Content i -> (String, String)
entryToTuple entry = (sideRepr side1, sideRepr side2)
  where (side1:side2:[]) = tag "entry" /> tag "side" $ entry

sideRepr :: Content i -> String
sideRepr side = show . htmlprint $ tag "side" /> tag "repr" $ side

main :: IO ()
main = do
  searchFor:_ <- getArgs
  query <- simpleHTTP . getRequest . buildLeoUrl $ searchFor
  queryResult <- getResponseBody query
  let document = xmlParse "" queryResult
  let content = docContent (posInNewCxt "" Nothing) document
  let sections = sectionsFilter content
  putStrLn $ prettySections sections
