module CmdArgs (
    parseArguments
  , testFile
  , reverseOutput
  , argsRest
  , OutputFormat(..)
  , outputFormat
  ) where

import System.Console.ParseArgs (
    Arg(Arg)
  , Argtype(..)
  , argDataOptional
  , parseArgs
  , ArgsComplete(ArgsTrailing)
  , Args
  , getArg
  , gotArg
  , argsRest
  )


data OutputFormat = Pretty | Xml deriving (Show)

data ArgumentKeys = File | Reverse | XmlOut deriving (Show, Ord, Eq)

allArguments :: [Arg ArgumentKeys]
allArguments =
  [
    Arg File (Just 'f') (Just "file") (argDataOptional "FILE" ArgtypeString)
        "work with FILE instead of a HTTP-Request"
  , Arg Reverse (Just 'r') (Just "reverse-output") Nothing
        "reverses the program output"
  , Arg XmlOut (Just 'x') (Just "show-xml-response") Nothing
        "show XML response instead of parsed Translations"
  ]

parseArguments :: String -> [String] -> Args ArgumentKeys
parseArguments = parseArgs (ArgsTrailing "SEARCH") allArguments


testFile :: Args ArgumentKeys-> Maybe String
testFile args = getArg args File


reverseOutput :: Args ArgumentKeys  -> [a] -> [a]
reverseOutput args = if gotArg args Reverse then id else reverse


outputFormat :: Args ArgumentKeys -> OutputFormat
outputFormat args = if gotArg args XmlOut then Xml else Pretty
