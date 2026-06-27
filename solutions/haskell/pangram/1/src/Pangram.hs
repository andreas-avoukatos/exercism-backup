module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = all (\x -> elem x (map toLower text)) ['a'..'z']
