module CmdArgs (
                 parseArguments
               , testFile
               , reverseOutput
               , argsRest
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


allArguments :: [Arg Int]
allArguments =
  [
    Arg 0 (Just 't') (Just "test-file") (argDataOptional "FILE" ArgtypeString)
        "work with FILE instead of a HTTP-Request"
  , Arg 1 (Just 'r') (Just "reverse-output") Nothing
        "reverses the program output"
  ]

parseArguments :: String -> [String] -> Args Int
parseArguments = parseArgs (ArgsTrailing "SEARCH") allArguments


testFile :: Args Int -> Maybe String
testFile args = getArg args 0


reverseOutput :: Args Int -> [a] -> [a]
reverseOutput args = if gotArg args 1 then id else reverse
