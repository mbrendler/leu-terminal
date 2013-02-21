import System.Environment (getArgs, getProgName)
import Control.Monad (liftM2)

import Codec.Binary.UTF8.String (decodeString)

import CmdArgs (parseArguments, argsRest, testFile, reverseOutput)
import LeoHttpRequest (searchWithHttp)
import ParseLeo (xmlStringToParts)
import PrettyPart (prettyPart)


putLines :: [String] -> IO ()
putLines = putStrLn . decodeString . unlines


main :: IO ()
main = do
  opts <- liftM2 parseArguments getProgName getArgs
  
  let searchFor = unwords $ argsRest opts
  queryResult <- maybe (searchWithHttp searchFor) readFile (testFile opts)

  let parts = xmlStringToParts queryResult
  -- TODO: filter parts by command line options
  putLines $ map prettyPart $ reverseOutput opts parts
