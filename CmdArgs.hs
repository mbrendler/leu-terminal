module CmdArgs (
                 parseArguments
               , testFile
               ) where

import System.Console.ParseArgs (
                                  Arg(Arg)
                                , Argtype(..)
                                , argDataOptional
                                , parseArgs
                                , ArgsComplete(ArgsTrailing)
                                , Args
                                , getArg
                                )


allArguments :: [Arg Int]
allArguments = [
  Arg 0 (Just 't') (Just "test-file") (argDataOptional "FILE" ArgtypeString)
   "work with FILE instead of a HTTP-Request"
               ]

parseArguments :: String -> [String] -> Args Int
parseArguments = parseArgs (ArgsTrailing "SEARCH") allArguments


testFile :: Args Int -> Maybe String
testFile args = getArg args 0
