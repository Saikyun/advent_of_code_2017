import Data.List (sort)

isValid :: [String] -> Bool
isValid (w:ws) = not (w `elem` ws) && isValid ws
isValid _      = True

isValidNoAnagram :: String -> Bool
isValidNoAnagram s = isValid $ words s

isValidDespiteAnagram :: String -> Bool
isValidDespiteAnagram s = isValid $ map sort $ words s

main = do
  input <- readFile "fourth-input"
  print $ length $ filter (==True) $ map isValidNoAnagram (lines input)
  print $ length $ filter (==True) $ map isValidDespiteAnagram (lines input)

uglyTest1 = isValidNoAnagram "aa bb cc dd ee"
           && not (isValidNoAnagram "aa bb cc dd aa")
           && isValidNoAnagram "aa bb cc dd aaa"

uglyTest2 = isValidDespiteAnagram "abcde fghij"
            && not (isValidDespiteAnagram "abcde xyz ecdab")
            && isValidDespiteAnagram "a ab abc abd abf abj"
            && isValidDespiteAnagram "iiii oiii ooii oooi oooo"
            && not (isValidDespiteAnagram "oiii ioii iioi iiio")
