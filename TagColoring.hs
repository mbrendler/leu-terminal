{- Contains functions to colorize (SGR) the output by HTML / XML tags.
 - -}
module TagColoring (clearSGR, tagToSGR) where

import System.Console.ANSI (setSGRCode,
                            SGR(SetColor),
                            ConsoleLayer(Foreground),
                            ColorIntensity(Dull, Vivid),
                            Color(Blue, Yellow))


-- reset the SGR
clearSGR :: String
clearSGR = setSGRCode []

-- set SGR by given HTML / XML tag
tagToSGR :: String -> String
tagToSGR "b" = setSGRCode [SetColor Foreground Vivid Blue]
tagToSGR "small" = setSGRCode [SetColor Foreground Dull Yellow]
tagToSGR "i" = ""
tagToSGR "repr" = ""
tagToSGR x = "UNHANDLED TAGNAME (" ++ x ++ ")"
