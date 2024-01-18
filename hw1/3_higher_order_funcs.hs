-- hw1_3 part a
applyAll :: [b -> c] -> b -> [c]
applyAll funcs val = map (\func -> func val) funcs

-- hw1_3 part b
type Property a = [a -> Bool]
satisfies :: Property a -> a -> Bool
satisfies prop val = and $ applyAll prop val

-- hw1_3 part c
power :: ( a -> a) -> Int -> a -> a
power _ 0 x = x
power func num x = power func (num - 1) $ func x

-- hw1_3 part d
plus :: Int -> Int -> Int
plus a b
  | (a > 0) && (b > 0) = power succ a b
  | (a < 0) && (b > 0) = power pred (abs a) b
  | otherwise = power pred (abs b) a


times :: Int -> Int -> Int
times x y
  |( x < 0) == (y < 0) = power (plus (abs x)) (abs y) 0
  | otherwise = negate (power (plus (abs x)) (abs y) 0)

