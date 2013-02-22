module Leo.HttpRequest (searchWithHttp) where

import Data.List (intercalate)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody, Request)
import Network.HTTP.Headers (setHeaders, Header(Header), HeaderName(HdrCookie))


buildLeoUrl :: String -> String
buildLeoUrl searchFor = url ++ "?" ++ intercalate "&" arguments
  where
    url = "http://dict.leo.org/dictQuery/m-vocab/ende/query.xml"
    arguments = [
        "tolerMode=nof"
      , "lp=ende&lang=de"
      , "rmWords=off"
      , "rmSearch=on"
      , "search=" ++ searchFor
      , "searchLoc=0"
      , "resultOrder=basic"
      , "multiwordShowSingle=on"
      ]  -- I do not know the meaning of all this arguments

-- Maybe I need to get the Cookie by a regular call to dict.leo.org at first!
addHeaders :: Request a -> Request a
addHeaders r = setHeaders r [
    Header HdrCookie "LEOABTEST=T; browser=webkit%3B5%3Bajax"
  ]

searchWithHttp :: String -> IO String
searchWithHttp search = httpRequest search >>= getResponseBody
  where httpRequest = simpleHTTP . addHeaders . getRequest . buildLeoUrl
