module Leu.Pretty (prettyPart) where

import Data.List (intersperse)

import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Reference(..))
import System.Console.ANSI (setSGRCode,
                            SGR(SetColor),
                            ConsoleLayer(Foreground),
                            ColorIntensity(Dull, Vivid),
                            Color(Blue, Yellow, White, Red))

import Leu.Types (
    Part(Part, PartSimilar)
  , Translation(Translation)
  , Terminal(Terminal)
  )
import Leu.Utils (zipWithDefault)
import Leu.LineWrapper (
    TextPart(..)
  , textPartLen
  , wrap
  , wrapFillStart
  , showLines
  )

prettyPart :: Terminal -> Part i -> String
prettyPart (Terminal width colorSupport) = _prettyPart
  where
    clear = if colorSupport then clearSGR else ""
    tagConverter = if colorSupport then tagToSGR else const ""
    dullWhite = if colorSupport then colorCode Dull White else ""
    dullRed = if colorSupport then colorCode Dull Red else ""

    _prettyPart :: Part i -> String
    _prettyPart (Part direct section entries) = heading ++ "\n" ++ content
      where
        heading = show direct ++ ": " ++ section
        content = unlines [prettyEntry x | x <- reverse entries]
    _prettyPart (PartSimilar ws lang) = plang ++ " " ++ concat pwords
      where
        plang = dullWhite ++ lang ++ ":" ++ clear
        pwords = intersperse " - " [dullRed ++ w ++ clear | w <- ws]
    _prettyPart x = show x

    prettyEntry :: Translation i -> String
    prettyEntry (Translation l r) = showLines allLines
      where
        sep = TextPart "--" [] ""
        sepLenWithEnclosingSpaces = 2 + textPartLen sep
        oneSideWidth = (width - sepLenWithEnclosingSpaces) `div` 2
        leftDefault = TextPart (replicate oneSideWidth ' ') [] ""
        left = wrapFillStart oneSideWidth $ reprToTextPart l
        right = wrap oneSideWidth $ reprToTextPart r
        allLines = zipWithDefault ((++). (++ [sep])) [leftDefault] [] left right
    prettyEntry x = show x

    reprToTextPart :: Content i -> [TextPart]
    reprToTextPart = reprToTextPart' []

    contentsToTextParts :: [String] -> [Content i] -> [TextPart]
    contentsToTextParts o = concatMap (reprToTextPart' o)

    reprToTextPart' :: [String] -> Content i -> [TextPart]
    reprToTextPart' opts (CElem (Elem (N tagName) _ subs) _) =
      contentsToTextParts (tagConverter tagName : opts) subs
    reprToTextPart' opts (CElem (Elem (QN _ _) _ _) _) =
      [TextPart "Not handled: QN" opts clear]
    reprToTextPart' opts (CString _ s _) = [TextPart s opts clear]
    reprToTextPart' _ (CRef (RefEntity _) _) = []  -- "RefEntity: " ++ n
    reprToTextPart' _ (CRef (RefChar _) _) = []  -- "RefChar: " ++ show n
    reprToTextPart' opts (CMisc _ _) = [TextPart "Misc" opts clear]

-- reset the SGR
clearSGR :: String
clearSGR = setSGRCode []

-- set SGR by given HTML / XML tag
tagToSGR :: String -> String
tagToSGR "b" = colorCode Vivid Blue
tagToSGR "small" = colorCode Dull Yellow
tagToSGR "sup" = colorCode Dull White
tagToSGR "i" = ""
tagToSGR "repr" = ""
tagToSGR "br" = ""
tagToSGR "t" = ""
tagToSGR x = "UNHANDLED TAGNAME (" ++ x ++ ")"

colorCode :: ColorIntensity -> Color -> String
colorCode intensity color = setSGRCode [SetColor Foreground intensity color]
