import System.Environment (getArgs, getProgName)
import Control.Monad (liftM2)

import Codec.Binary.UTF8.String (decodeString)

import CmdArgs (
  Options,
  parseArguments,
  argsRest,
  testFile,
  OutputFormat(..),
  outputFormat,
  language,
  showLanguages
  )
import Leu.HttpRequest (searchWithHttp)
import Leu.Parse (xmlStringToParts)
import Leu.Pretty (prettyPart)
import Leu.Types (lDescription, allLanguageMappings, buildTerminal, Terminal)


putLines :: [String] -> IO ()
putLines = putStr . decodeString . unlines

getOutputLines :: Terminal -> OutputFormat -> String -> [String]
getOutputLines _ Xml queryResult = [queryResult]
getOutputLines terminal Pretty queryResult =
  let parts = xmlStringToParts queryResult
    in if null parts
       then ["No translation found.",
             "Use '-x' to show the XML response."]
       else map (prettyPart terminal) $ reverse parts


runProgram :: Options -> IO ()
runProgram opts = do
  let lang = language opts
  putStrLn $ "use language: " ++ show lang ++ " (" ++ lDescription lang ++ ")"

  let searchFor = unwords $ argsRest opts
  queryResult <- maybe (searchWithHttp searchFor lang) readFile (testFile opts)

  terminal <- buildTerminal
  putLines $ getOutputLines terminal (outputFormat opts) queryResult


languageMappingsString :: String
languageMappingsString = unlines $ map processOneLine allLanguageMappings
  where processOneLine x = show x ++ ": " ++ lDescription x


main :: IO ()
main = do
  opts <- liftM2 parseArguments getProgName getArgs
  if showLanguages opts
    then putStrLn languageMappingsString
    else runProgram opts
