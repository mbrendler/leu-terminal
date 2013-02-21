module CmdArgs (
    parseArguments
  , testFile
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

data ArgumentKeys = File | XmlOut deriving (Show, Ord, Eq)

allArguments :: [Arg ArgumentKeys]
allArguments =
  [
    Arg File (Just 'f') (Just "file") (argDataOptional "FILE" ArgtypeString)
        "work with FILE instead of a HTTP-Request"
  , Arg XmlOut (Just 'x') (Just "show-xml-response") Nothing
        "show XML response instead of parsed Translations"
  ]

parseArguments :: String -> [String] -> Args ArgumentKeys
parseArguments = parseArgs (ArgsTrailing "SEARCH") allArguments

testFile :: Args ArgumentKeys-> Maybe String
testFile args = getArg args File

outputFormat :: Args ArgumentKeys -> OutputFormat
outputFormat args = if gotArg args XmlOut then Xml else Pretty
