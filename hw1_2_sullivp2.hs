-- import Data.List.NonEmpty (NonEmpty)
type Row = [Int]
type Column = [Int]
type Matrix = [Row]

tMat :: Matrix = [[1,2,3], [4,5,6], [7,8,9]]

-- hw1_2 part a
-- vAddNaive 

-- vAdd :: NonEmpty Row -> NonEmpty Row -> Maybe Row
-- vAdd v1 v2
--   | length v1 == length v2  = Just(sum $ zipWith (*) v1 v2)
--   | otherwise = Nothing

-- -- hw1_2 part b
-- mAdd :: Matrix -> Matrix -> Matrix
-- mAdd = zipWith vAdd

-- --hw1_2 part c
-- trow :: Row = [1, 6, 9]
-- tcol :: Column = [11, 13, 15]
-- iProd :: Row -> Column -> Int
-- iProd row col = sum  $ zipWith (*) row col
