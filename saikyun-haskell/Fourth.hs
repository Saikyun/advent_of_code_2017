isValid :: String -> Bool
isValid s = isValid' (words s)
  where
    isValid' (w:ws) = not (w `elem` ws) && isValid' ws
    isValid' _      = True

main = do
  input <- readFile "fourth-input"
  print $ length $ filter (\x -> x) $ map isValid (lines input)

uglyTest = isValid "aa bb cc dd ee"
           && not (isValid "aa bb cc dd aa")
           && isValid "aa bb cc dd aaa"
