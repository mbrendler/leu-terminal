
module Leu.LineWrapper (
  TextPart(..),
  textPartLen,
  textPartsLen,
  showWords,
  showLines,
  wrap,
  wrapFillStart,
  ) where

import Data.List (intercalate, intersperse)


type TextOptions = [String]
type EndOption = String

data TextPart = TextPart String TextOptions EndOption | Space

instance Show TextPart where
  show (TextPart s opts endOpt) = concat opts ++ s ++ endOpt
  show Space = " "

spacesTextPart :: Int -> TextPart
spacesTextPart n = TextPart (replicate n ' ') [] ""

showWords :: [TextPart] -> String
showWords = concatMap show . intersperse Space

showLines :: [[TextPart]] -> String
showLines = intercalate "\n" . map showWords


textPartLen :: TextPart -> Int
textPartLen (TextPart s _ _) = length s
textPartLen Space = 1

textPartsLen :: [TextPart] -> Int
textPartsLen = sum . map textPartLen . intersperse Space


wrap :: Int -> [TextPart] -> [[TextPart]]
wrap n = combineLineParts . splitLongParts
  where
    splitLongParts = foldl splitLongPart []
    splitLongPart x (TextPart s y z) =
      x ++ (if length s > n
            then map (\a -> TextPart a y z) (chop s)
            else [TextPart s y z])
    splitLongPart _ Space = [Space]
    chop "" = []
    chop s = start : chop rest
      where (start,rest) = splitAt n s

    combineLineParts textParts = inner textParts [] [] 0
      where
        inner [] r m _ = reverse (reverse m:r)
        inner (t:ts) r m l
          | n >= lineLength = inner ts r (t:m) lineLength
          | otherwise = inner (t:ts) (reverse m:r) [] 0
          where lineLength = textPartLen t + l + if l > 0 then 1 else 0


wrapFillStart :: Int -> [TextPart] -> [[TextPart]]
wrapFillStart n = map addStart . wrap n
  where
    addStart :: [TextPart] -> [TextPart]
    addStart x
      | textPartsLen x < n = spacesTextPart (n - textPartsLen x - 1) : x
      | otherwise = x
