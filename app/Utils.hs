module Utils (splitOn, trimStart) where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s =  case dropWhile (==c) s of
                    [] -> []
                    s' -> w : splitOn c s''
                          where (w, s'') = break (==c) s'

trimStart :: String -> String
trimStart "" = ""
trimStart (x:xs)
  | x == ' ' = trimStart xs
  | otherwise = x:xs
