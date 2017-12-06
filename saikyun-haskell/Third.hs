--------- 3a ----------------------------------------

data Threshold =
  Threshold { distance :: Int
            , border :: Int
              }
  deriving (Show)

biggest (Threshold _ b) = b^2
smallest (Threshold _ 1) = 1
smallest (Threshold _ b) = ((b-2)^2)+1

thresholds :: [Threshold]
thresholds = [ Threshold { distance = dist, border = colrow } | dist <- [0..], let colrow = head $ drop dist $ [1, 3..]]

threshByNum :: Int -> Threshold
threshByNum i = head $ dropWhile (\t -> biggest t < i) thresholds

coord :: Threshold -> Int -> Int
coord _ 1 = 0
coord t@(Threshold d b) i = d + offset
  where
    pos  = i - smallest t
    side | d == 0 = 0
         | otherwise = abs $ pos `mod` (d * 2)
    offset = abs $ side - (d - 1)

stepCount :: Int -> Int
stepCount i = coord (threshByNum i) i


uglyTest1 = (stepCount 1 == 0)
            && stepCount 12 == 3
            && stepCount 23 == 2
            && stepCount 1024 == 31


--------- 3a slut ---------------------------------

