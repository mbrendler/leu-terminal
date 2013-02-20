import System.Environment (getArgs, getProgName)
import Control.Monad (liftM2)
import Codec.Binary.UTF8.String (decodeString)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody, Request)
import Network.HTTP.Headers (setHeaders, Header(Header), HeaderName(HdrCookie))

import ParseLeo (xmlStringToParts)
import PrettyPart (prettyPart)
import CmdArgs (parseArguments, testFile)
import System.Console.ParseArgs (argsRest)


buildLeoUrl :: String -> String
buildLeoUrl searchFor = "http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search=" ++ searchFor ++ "&searchLoc=0&resultOrder=basic&multiwordShowSingle=on"

-- Maybe I need to get the Cookie by a regular call to dict.leo.org at first!
addHeaders :: Request a -> Request a
addHeaders r = setHeaders r [Header HdrCookie "LEOABTEST=T; browser=webkit%3B5%3Bajax"]

searchWithHttp :: String -> IO String
searchWithHttp search = httpRequest search >>= getResponseBody
  where httpRequest = simpleHTTP . addHeaders . getRequest . buildLeoUrl


main :: IO ()
main = do
  opts <- liftM2 parseArguments getProgName getArgs
  
  let searchFor = unwords $ argsRest opts
  queryResult <- maybe (searchWithHttp searchFor) readFile (testFile opts)

  let parts = xmlStringToParts queryResult
  -- TODO: filter parts by command line options
  putStrLn $ decodeString $ unlines $ map prettyPart $ reverse parts
