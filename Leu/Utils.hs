
module Leu.Utils (toLowerCase, zipWithDefault) where

import Data.Char (toLower)

toLowerCase :: String -> String
toLowerCase = map toLower

zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault _ _ _ [] [] = []
zipWithDefault f da db [] (b:bs) = f da b : zipWithDefault f da db [] bs
zipWithDefault f da db (a:as) [] = f a db : zipWithDefault f da db as []
zipWithDefault f da db (a:as) (b:bs) = f a b : zipWithDefault f da db as bs
