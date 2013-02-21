module PrettyPart (prettyPart) where

import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Reference(..))
import System.Console.ANSI (setSGRCode,
                            SGR(SetColor),
                            ConsoleLayer(Foreground),
                            ColorIntensity(Dull, Vivid),
                            Color(Blue, Yellow))

import ParseLeo (Part(Part), Translation(Translation))


prettyPart :: Part i -> String
prettyPart (Part direct section entries) =
  show direct ++ ": " ++ section ++ "\n" ++ unlines (map prettyEntry entries)
prettyPart x = show x

prettyEntry :: Translation i -> String
prettyEntry (Translation a b) = reprToString a ++ " --- " ++ reprToString b
prettyEntry x = show x

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
tagToSGR "b" = setSGRCode [SetColor Foreground Vivid Blue]
tagToSGR "small" = setSGRCode [SetColor Foreground Dull Yellow]
tagToSGR "i" = ""
tagToSGR "repr" = ""
tagToSGR x = "UNHANDLED TAGNAME (" ++ x ++ ")"