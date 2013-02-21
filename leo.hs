import System.Environment (getArgs, getProgName)
import Control.Monad (liftM2)

import Codec.Binary.UTF8.String (decodeString)

import CmdArgs (parseArguments, argsRest, testFile, OutputFormat(..),
                outputFormat)
import Leo.HttpRequest (searchWithHttp)
import Leo.Parse (xmlStringToParts)
import Leo.Pretty (prettyPart)


putLines :: [String] -> IO ()
putLines = putStrLn . decodeString . unlines


getOutputLines :: OutputFormat -> String -> [String]
getOutputLines Xml queryResult = [queryResult]
getOutputLines Pretty queryResult =
  let parts = xmlStringToParts queryResult
    in if null parts
       then ["No translation found.",
             "Use '-x' to show the XML response."]
       else map prettyPart $ reverse parts


main :: IO ()
main = do
  opts <- liftM2 parseArguments getProgName getArgs
  
  let searchFor = unwords $ argsRest opts
  queryResult <- maybe (searchWithHttp searchFor) readFile (testFile opts)

  putLines $ getOutputLines (outputFormat opts) queryResult
