module Leo.Pretty (prettyPart) where

import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Reference(..))
import Data.List.Utils (replace)
import System.Console.ANSI (setSGRCode,
                            SGR(SetColor),
                            ConsoleLayer(Foreground),
                            ColorIntensity(Dull, Vivid),
                            Color(Blue, Yellow, White))

import Leo.Types (Part(Part), Translation(Translation))


prettyPart :: Int -> Part i -> String
prettyPart width (Part direct section entries) = heading ++ "\n" ++ content
  where
    heading = show direct ++ ": " ++ section
    content = unlines $ map (prettyEntry width) $ reverse entries
prettyPart _ x = show x

stripSGR :: String -> String
stripSGR s = foldl (\m r -> replace r "" m) s allSGRCodes

textLength :: String -> Int
textLength = length . stripSGR

prettyEntry :: Int -> Translation i -> String
prettyEntry textWidth (Translation l r) = if textLength simpleLine <= textWidth
                                then simpleLine
                                else multiLine
  where
    left = reprToString l
    sep = " -- "
    sideWith = (textWidth - length sep) `div` 2
    leftSpacesCount = sideWith - textLength left
    left' = if leftSpacesCount > 0
            then replicate leftSpacesCount ' ' ++ left
            else left
    simpleLine = left' ++ sep ++ reprToString r
    multiLine = left ++ "\n " ++ sep ++ reprToString r
prettyEntry _ x = show x

contentsToString :: [Content i] -> String
contentsToString = concatMap reprToString

reprToString :: Content i -> String
reprToString (CElem (Elem (N tagName) _ subs) _) =
  tagToSGR tagName ++ contentsToString subs ++ clearSGR
reprToString (CElem (Elem (QN _ _) _ _) _) = "Not handled: QN"
reprToString (CString _ s _) = s
reprToString (CRef (RefEntity _) _) = ""  -- "RefEntity: " ++ n
reprToString (CRef (RefChar _) _) = ""  -- "RefChar: " ++ show n
reprToString (CMisc _ _) = "Misc"


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
tagToSGR x = "UNHANDLED TAGNAME (" ++ x ++ ")"

colorCodeBlue :: String
colorCodeBlue = setSGRCode [SetColor Foreground Vivid Blue]
colorCodeYellow :: String
colorCodeYellow = setSGRCode [SetColor Foreground Dull Yellow]
colorCodeWhite :: String
colorCodeWhite = setSGRCode [SetColor Foreground Dull White]

allSGRCodes :: [String]
allSGRCodes = [clearSGR, colorCodeBlue, colorCodeYellow, colorCodeWhite]
