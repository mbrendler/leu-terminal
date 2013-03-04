module CmdArgs (
    Options
  , parseArguments
  , testFile
  , argsRest
  , OutputFormat(..)
  , outputFormat
  , language
  , showLanguages 
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

data ArgumentKeys = File | XmlOut | Lang | ShowLangs deriving (Show, Ord, Eq)

type Options = Args ArgumentKeys

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
  , Arg ShowLangs Nothing (Just "show-languages") Nothing
        "show all supported langages"
  ]

parseArguments :: String -> [String] -> Args ArgumentKeys
parseArguments = parseArgs (ArgsTrailing "SEARCH") allArguments

testFile :: Args ArgumentKeys-> Maybe String
testFile args = getArg args File

outputFormat :: Args ArgumentKeys -> OutputFormat
outputFormat args = if gotArg args XmlOut then Xml else Pretty

language :: Args ArgumentKeys -> LanguageMapping
language args = maybe EnDe readLang (getArg args Lang)

showLanguages :: Args ArgumentKeys -> Bool
showLanguages args = gotArg args ShowLangs
