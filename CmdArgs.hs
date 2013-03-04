module CmdArgs (
    parseArguments
  , testFile
  , argsRest
  , OutputFormat(..)
  , outputFormat
  , language
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

import Leu.Types (
    LanguageMapping(..)
  , readLang
  )


data OutputFormat = Pretty | Xml deriving (Show)

data ArgumentKeys = File | XmlOut | Lang deriving (Show, Ord, Eq)

allArguments :: [Arg ArgumentKeys]
allArguments =
  [
    Arg File (Just 'f') (Just "file") (argDataOptional "FILE" ArgtypeString)
        "work with FILE instead of a HTTP-Request"
  , Arg XmlOut (Just 'x') (Just "show-xml-response") Nothing
        "show XML response instead of parsed Translations"
  , Arg Lang (Just 'l') (Just "language")
        (argDataOptional "LANG" ArgtypeString)
        "use another language default is ende"
  ]

parseArguments :: String -> [String] -> Args ArgumentKeys
parseArguments = parseArgs (ArgsTrailing "SEARCH") allArguments

testFile :: Args ArgumentKeys-> Maybe String
testFile args = getArg args File

outputFormat :: Args ArgumentKeys -> OutputFormat
outputFormat args = if gotArg args XmlOut then Xml else Pretty

language :: Args ArgumentKeys -> LanguageMapping
language args = maybe EnDe readLang (getArg args Lang)
