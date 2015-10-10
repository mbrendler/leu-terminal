import System.Environment (getArgs, getProgName)
import System.IO (stdout)
import Control.Monad (liftM2)

import Codec.Binary.UTF8.String (decodeString)

import System.Console.ANSI (hSupportsANSI)
import CmdArgs (Options, parseArguments, argsRest, testFile, OutputFormat(..),
                outputFormat, language, showLanguages)
import TermSize (getTermSize)
import Leu.HttpRequest (searchWithHttp)
import Leu.Parse (xmlStringToParts)
import Leu.Pretty (prettyPart)
import Leu.Types (lDescription, allLanguageMappings)


putLines :: [String] -> IO ()
putLines = putStr . decodeString . unlines


getOutputLines :: Int -> Bool -> OutputFormat -> String -> [String]
getOutputLines _ _ Xml queryResult = [queryResult]
getOutputLines termWidth colorSupport Pretty queryResult =
  let parts = xmlStringToParts queryResult
    in if null parts
       then ["No translation found.",
             "Use '-x' to show the XML response."]
       else map (prettyPart colorSupport termWidth) $ reverse parts


runProgram :: Options -> IO ()
runProgram opts = do
  let lang = language opts
  putStrLn $ "use language: " ++ show lang ++ " (" ++ lDescription lang ++ ")"

  let searchFor = unwords $ argsRest opts
  queryResult <- maybe (searchWithHttp searchFor lang) readFile (testFile opts)

  (_, termWidth) <- getTermSize (Just (25, 80))
  colorSupport <- hSupportsANSI stdout
  putLines $ getOutputLines termWidth colorSupport (outputFormat opts) queryResult


languageMappingsString :: String
languageMappingsString = unlines $ map processOneLine allLanguageMappings
  where processOneLine x = show x ++ ": " ++ lDescription x


main :: IO ()
main = do
  opts <- liftM2 parseArguments getProgName getArgs
  if showLanguages opts
    then putStrLn languageMappingsString
    else runProgram opts
