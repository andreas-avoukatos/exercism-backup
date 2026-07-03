module CollatzConjecture (collatz) where


maybeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeAdd (Just a) (Just b) = Just (a + b)
maybeAdd _ _ = Nothing

  
collatz :: Integer -> Maybe Integer
collatz n | mod n 2 == 0 && n > 0  = maybeAdd (Just 1) (collatz (div n 2))
          | n == 1 = Just 0
          | mod n 2 == 1 = maybeAdd (Just 1) (collatz (3*n + 1))
          | otherwise = Nothing
