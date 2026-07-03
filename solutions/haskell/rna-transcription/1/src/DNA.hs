module DNA (toRNA) where

toNucleotide :: Char -> Either Char Char
toNucleotide 'G' = Right 'C'
toNucleotide 'C' = Right 'G'
toNucleotide 'T' = Right 'A'
toNucleotide 'A' = Right 'U'
toNucleotide ch = Left ch

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA (x : xs) = do
  rnaChar <- toNucleotide x
  rest <- toRNA xs
  return (rnaChar : rest)
