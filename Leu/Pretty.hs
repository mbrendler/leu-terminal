module Leu.Pretty (prettyPart) where

import Data.List (intersperse)

import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Reference(..))
import System.Console.ANSI (setSGRCode,
                            SGR(SetColor),
                            ConsoleLayer(Foreground),
                            ColorIntensity(Dull, Vivid),
                            Color(Blue, Yellow, White, Red))

import Leu.Types (Part(Part, PartSimilar), Translation(Translation))
import Leu.Utils (zipWithDefault)
import Leu.LineWrapper (
    TextPart(..)
  , textPartLen
  , wrap
  , wrapFillStart
  , showLines
  )


prettyPart :: Int -> Part i -> String
prettyPart width (Part direct section entries) = heading ++ "\n" ++ content
  where
    heading = show direct ++ ": " ++ section
    content = unlines [prettyEntry width x | x <- reverse entries]
prettyPart _ (PartSimilar ws lang) = plang ++ " " ++ concat pwords
  where  -- TODO: check line width
    plang = colorCodeWhite ++ lang ++ ":" ++ clearSGR
    pwords = intersperse " - " [colorCodeRed ++ w ++ clearSGR | w <- ws]
prettyPart _ x = show x

prettyEntry :: Int -> Translation i -> String
prettyEntry textWidth (Translation l r) = showLines allLines
  where
    sep = TextPart "--" [] ""
    oneSideWidth = (textWidth - (2 + textPartLen sep)) `div` 2
    leftDefault = TextPart (replicate oneSideWidth ' ') [] ""
    left = wrapFillStart oneSideWidth $ reprToTextPart l
    right = wrap oneSideWidth $ reprToTextPart r
    allLines = zipWithDefault ((++). (++ [sep])) [leftDefault] [] left right
prettyEntry _ x = show x


reprToTextPart :: Content i -> [TextPart]
reprToTextPart = reprToTextPart' []

contentsToTextParts :: [String] -> [Content i] -> [TextPart]
contentsToTextParts o = concatMap (reprToTextPart' o)

reprToTextPart' :: [String] -> Content i -> [TextPart]
reprToTextPart' opts (CElem (Elem (N tagName) _ subs) _) =
  contentsToTextParts (tagToSGR tagName : opts) subs
reprToTextPart' opts (CElem (Elem (QN _ _) _ _) _) =
  [TextPart "Not handled: QN" opts clearSGR]
reprToTextPart' opts (CString _ s _) = [TextPart s opts clearSGR]
reprToTextPart' _ (CRef (RefEntity _) _) = []  -- "RefEntity: " ++ n
reprToTextPart' _ (CRef (RefChar _) _) = []  -- "RefChar: " ++ show n
reprToTextPart' opts (CMisc _ _) = [TextPart "Misc" opts clearSGR]


-- reset the SGR
clearSGR :: String
clearSGR = setSGRCode []

-- set SGR by given HTML / XML tag
tagToSGR :: String -> String
tagToSGR "b" = colorCodeBlue
tagToSGR "small" = colorCodeYellow
tagToSGR "sup" = colorCodeWhite
tagToSGR "i" = ""
tagToSGR "repr" = ""
tagToSGR "br" = ""
tagToSGR x = "UNHANDLED TAGNAME (" ++ x ++ ")"

colorCodeBlue :: String
colorCodeBlue = setSGRCode [SetColor Foreground Vivid Blue]
colorCodeYellow :: String
colorCodeYellow = setSGRCode [SetColor Foreground Dull Yellow]
colorCodeWhite :: String
colorCodeWhite = setSGRCode [SetColor Foreground Dull White]
colorCodeRed :: String
colorCodeRed = setSGRCode [SetColor Foreground Dull Red]
