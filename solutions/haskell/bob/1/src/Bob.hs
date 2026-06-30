{-# LANGUAGE OverloadedStrings #-}
module Bob (responseFor) where

import Data.List (dropWhileEnd)
import Data.Char

responseFor :: String -> String
responseFor txt | isShouting && isQuestion = "Calm down, I know what I'm doing!"
                | isShouting = "Whoa, chill out!"
                | isQuestion = "Sure."
                | isSilence = "Fine. Be that way!"
                | otherwise = "Whatever."
  where
    strippedtxt = dropWhileEnd isSpace txt
    isSilence  = all isSpace strippedtxt
    isQuestion = not (null strippedtxt)  && last strippedtxt == '?'
    isShouting = any isUpper strippedtxt && not (any isLower strippedtxt)
