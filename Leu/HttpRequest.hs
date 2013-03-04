module Leu.HttpRequest (searchWithHttp) where

import Data.List (intercalate)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody, Request)
import Network.HTTP.Headers (setHeaders, Header(Header), HeaderName(HdrCookie))
import Network.HTTP.Base (urlEncode)

import Leu.Types (LanguageMapping)
import Leu.Utils (toLowerCase)


buildLeoUrl :: String -> LanguageMapping -> String
buildLeoUrl searchFor lang = url ++ "?" ++ intercalate "&" arguments
  where
    langStr = toLowerCase $ show lang
    url = "http://dict.leo.org/dictQuery/m-vocab/" ++ langStr ++ "/query.xml"
    arguments = [
        "tolerMode=nof"
      , "lp=" ++ langStr
      , "lang=de"
      , "rmWords=off"
      , "rmSearch=on"
      , "search=" ++ urlEncode searchFor
      , "searchLoc=0"
      , "resultOrder=basic"
      , "multiwordShowSingle=on"
      ]  -- I do not know the meaning of all this arguments

-- Maybe I need to get the Cookie by a regular call to dict.leo.org at first!
addHeaders :: Request a -> Request a
addHeaders r = setHeaders r [
    Header HdrCookie "LEOABTEST=T; browser=webkit%3B5%3Bajax"
  ]

searchWithHttp :: String -> LanguageMapping -> IO String
searchWithHttp search lang = httpRequest >>= getResponseBody
  where
    httpRequest = simpleHTTP . addHeaders . getRequest $ buildLeoUrl search lang
