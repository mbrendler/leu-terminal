
module Leu.Utils (
    toLowerCase
  ) where

import Data.Char (toLower)


toLowerCase :: String -> String
toLowerCase = map toLower
