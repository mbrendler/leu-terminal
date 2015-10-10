import Leu.LineWrapper (
  TextPart(..),
  showLines,
  wrap,
  wrapFillStart,
  )

createSimpleParts :: String -> [TextPart]
createSimpleParts = map (\x -> TextPart x [] "") . words

test :: (Show d, Eq a, Show a) => d -> a -> a -> IO ()
test l x y
  | x == y = putStrLn $ show l ++ ": OK"
  | otherwise = putStrLn $ show l ++ ": " ++ show x ++ " != " ++ show y

testConverter :: (Show d, Eq b, Show b) => d -> b -> (a -> b) -> a -> IO ()
testConverter d e f t = test d e (f t)

testWrap :: String -> String -> Int -> String -> IO ()
testWrap d e w = testConverter d e converter
  where converter = showLines . wrap w . createSimpleParts

testWrapFillStart :: String -> String -> Int -> String -> IO ()
testWrapFillStart d e w = testConverter d e converter
  where converter = showLines . wrapFillStart w . createSimpleParts

main :: IO ()
main = do
  testWrap "1" "12\n34\n5" 2 "12345"
  testWrap "2" "1234\n78\n90a a" 5 "1234 78 90a a"
  testWrapFillStart "3" "  12" 4 "12"
  testWrapFillStart "4" "  12\n 345" 4 "12 345"
