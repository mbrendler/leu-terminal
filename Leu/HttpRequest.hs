{-# LANGUAGE OverloadedStrings #-}
module Leu.HttpRequest (searchWithHttp) where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.UTF8 (fromString)

import Leu.Types (LanguageMapping)
import Leu.Utils (toLowerCase)

import Network.HTTP.Simple (
  Request,
  httpLBS,
  parseRequest_,
  getResponseBody,
  getResponseStatusCode,
  setRequestHeaders,
  setRequestQueryString,
  )

-- dict.leo.org is behind Cloudflare, which answers requests that do not look
-- like a browser with '403 Forbidden'.  A plausible user-agent alone is not
-- enough, the modern browser headers below are needed as well.  Only 'gzip' is
-- accepted as content encoding - http-client decompresses that transparently,
-- but it cannot handle 'br'.
userAgent :: String
userAgent =
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) \
  \Chrome/126.0.0.0 Safari/537.36"

buildLeoRequest :: String -> LanguageMapping -> Request
buildLeoRequest searchFor lang =
    setRequestHeaders headers $ setRequestQueryString query $ parseRequest_ url
  where
    langStr = toLowerCase $ show lang
    url = "https://dict.leo.org/dictQuery/m-vocab/" ++ langStr ++ "/query.xml"
    query = [
        ("lp", Just $ fromString langStr)
      , ("search", Just $ fromString searchFor)
      , ("side", Just "both")
      , ("order", Just "basic")
      , ("partial", Just "show")
      , ("filtered", Just "-1")
      ]
    headers = [
        ("User-Agent", fromString userAgent)
      , ("Accept", "*/*")
      , ("Accept-Language", "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7")
      , ("Accept-Encoding", "gzip")
      , ("sec-ch-ua", "\"Chromium\";v=\"126\", \"Not.A/Brand\";v=\"24\"")
      , ("sec-ch-ua-mobile", "?0")
      , ("sec-ch-ua-platform", "\"Linux\"")
      , ("Sec-Fetch-Dest", "empty")
      , ("Sec-Fetch-Mode", "cors")
      , ("Sec-Fetch-Site", "same-origin")
      , ("Referer", "https://dict.leo.org/")
      , ("X-Requested-With", "XMLHttpRequest")
      ]

searchWithHttp :: String -> LanguageMapping -> IO String
searchWithHttp search lang = do
  response <- httpLBS $ buildLeoRequest search lang
  case getResponseStatusCode response of
    200 -> return $ unpack $ getResponseBody response
    code -> ioError $ userError $ "dict.leo.org returned HTTP " ++ show code
