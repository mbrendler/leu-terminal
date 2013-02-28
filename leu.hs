import System.Environment (getArgs, getProgName)
import Control.Monad (liftM2)

import Codec.Binary.UTF8.String (decodeString)

import CmdArgs (parseArguments, argsRest, testFile, OutputFormat(..),
                outputFormat)
import TermSize (getTermSize)
import Leu.HttpRequest (searchWithHttp)
import Leu.Parse (xmlStringToParts)
import Leu.Pretty (prettyPart)


putLines :: [String] -> IO ()
putLines = putStr . decodeString . unlines


getOutputLines :: Int -> OutputFormat -> String -> [String]
getOutputLines _ Xml queryResult = [queryResult]
getOutputLines termWidth Pretty queryResult =
  let parts = xmlStringToParts queryResult
    in if null parts
       then ["No translation found.",
             "Use '-x' to show the XML response."]
       else map (prettyPart termWidth) $ reverse parts


main :: IO ()
main = do
  opts <- liftM2 parseArguments getProgName getArgs
  
  let searchFor = unwords $ argsRest opts
  queryResult <- maybe (searchWithHttp searchFor) readFile (testFile opts)

  (_, termWidth) <- getTermSize
  putLines $ getOutputLines termWidth (outputFormat opts) queryResult
