module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

parseNucleotide :: Char -> Either String Nucleotide
parseNucleotide 'A' = Right A
parseNucleotide 'C' = Right C
parseNucleotide 'G' = Right G
parseNucleotide 'T' = Right T
parseNucleotide _ = Left "Invalid nucleotide"

emptyCounts :: Map Nucleotide Int
emptyCounts = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

countRec :: String -> Map Nucleotide Int -> Either String (Map Nucleotide Int)
countRec [] currentMap = Right currentMap
countRec (x : xs) currentMap =
  case parseNucleotide x of
    Left err -> Left err
    Right n ->
      let nextMap = Map.adjust (+ 1) n currentMap
       in countRec xs nextMap

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = countRec xs emptyCounts
