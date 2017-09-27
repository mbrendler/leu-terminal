module Leu.HttpRequest (searchWithHttp) where

import Data.List (intercalate)

import Data.ByteString.Lazy.Char8 (unpack)

import Leu.Types (LanguageMapping)
import Leu.Utils (toLowerCase)

import Network.HTTP.Simple (httpLBS, parseRequest_, getResponseBody)

buildLeoUrl :: String -> LanguageMapping -> String
buildLeoUrl searchFor lang = url ++ "?" ++ intercalate "&" arguments
  where
    langStr = toLowerCase $ show lang
    url = "http://dict.leo.org/dictQuery/m-vocab/" ++ langStr ++ "/query.xml"
    arguments = [
        "lp=" ++ langStr
      , "search=" ++ searchFor
      , "side=both"
      , "order=basic"
      , "partial=show"
      , "filtered=-1"
      ]

searchWithHttp :: String -> LanguageMapping -> IO String
searchWithHttp search lang = do
  let url = buildLeoUrl search lang
  response <- httpLBS $ parseRequest_ url
  return $ unpack $ getResponseBody response
