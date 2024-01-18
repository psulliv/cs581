import Data.List (find)

-- hw1_1 part a
type Bag a = [(a, Int)]

inBag :: Eq a => a -> Bag a -> Bool
inBag el bag = el `elem` map fst bag

countEl :: Eq a => a -> Bag a -> Int
countEl el bag = case find (\(x, y) -> x == el) bag of
  Just (x,y) -> y
  Nothing -> 0

ins :: Eq a => a -> Bag a -> Bag a
ins el bag
  | inBag el bag     = [if x == el then (x, y+1) else (x,y) | (x, y) <- bag ]
  | not (inBag el bag) = (el, 1):bag

tBag :: Bag Int = [(5,1), (7,3), (2,1), (3,2), (8,1)]
tl :: [Int] = [1, 3, 4, 5, 5, 1]

--hw1_1 part b
del :: Eq a => a -> Bag a -> Bag a
del el bag
  | countEl el bag > 1 = [if x == el then (x, y-1) else (x,y) | (x, y) <- bag ]
  | countEl el bag == 1 = [(x,y) | (x, y) <- bag, x /= el ]
  | countEl el bag == 0 = bag


--hw1_1 part c
bag :: Eq a => [a] -> Bag a
bag = foldr ins []

--hw1_1 part d
sBag1 :: Bag Int = [(5,1), (7,3), (2,1), (3,2), (8,1)]
sBag2 :: Bag Int = [(5,1), (7,3)]
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag b1 b2 = and [y <= countEl x b2| (x,y) <- b1]

--hw1_1 part e
isSet :: Eq a => Bag a -> Bool
isSet bag = and [y == 1| (_, y) <- bag]

--hw1_1 part f
size :: Bag a -> Int
size bag = sum (map snd bag)
