import System.Environment (getArgs, getProgName)
import Control.Monad (liftM2)

import Codec.Binary.UTF8.String (decodeString)
import System.Console.ParseArgs (argsRest)

import ParseLeo (xmlStringToParts)
import PrettyPart (prettyPart)
import CmdArgs (parseArguments, testFile, reverseOutput)
import LeoHttpRequest (searchWithHttp)


main :: IO ()
main = do
  opts <- liftM2 parseArguments getProgName getArgs
  
  let searchFor = unwords $ argsRest opts
  queryResult <- maybe (searchWithHttp searchFor) readFile (testFile opts)

  let parts = xmlStringToParts queryResult
  -- TODO: filter parts by command line options
  -- TODO: check if decodeString is really needed
  putStrLn . decodeString . unlines $ map prettyPart $ reverseOutput opts parts
