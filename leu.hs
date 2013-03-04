import System.Environment (getArgs, getProgName)
import Control.Monad (liftM2)

import Codec.Binary.UTF8.String (decodeString)

import CmdArgs (parseArguments, argsRest, testFile, OutputFormat(..),
                outputFormat, language)
import TermSize (getTermSize)
import Leu.HttpRequest (searchWithHttp)
import Leu.Parse (xmlStringToParts)
import Leu.Pretty (prettyPart)
import Leu.Types (lDescription)


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
  
  let lang = language opts
  putStrLn $ "use language: " ++ show lang ++ " (" ++ lDescription lang ++ ")"
  
  let searchFor = unwords $ argsRest opts
  queryResult <- maybe (searchWithHttp searchFor lang) readFile (testFile opts)

  (_, termWidth) <- getTermSize
  putLines $ getOutputLines termWidth (outputFormat opts) queryResult
