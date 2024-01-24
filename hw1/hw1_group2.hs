---------------------------------------------------------------------------------------------------
-- Question 1 - Lists

-- definition of bag/multiset:
-- can be represented as list of pairs (x, n) where n indicates the number of occurrences of x in the multiset.
type Bag a = [(a,Int)]

-- (a) Define the function ins that inserts an element into a multiset
-- The class constraint ”Eq a =>” restricts the element type a to those types that allow the comparison of elements for equality with ==
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x, 1)]  -- If the bag is empty, add the new element with count 1
ins x ((y, count):ys)
  | x == y    = (y, count + 1) : ys  -- If the element is found, increment the count
  | otherwise = (y, count) : ins x ys  -- Otherwise, continue searching in the rest of the bag


-- (b) Define the function del that removes a single element from a multiset. Note that deleting 3 from {2, 3, 3, 4}
-- yields {2, 3, 4} whereas deleting 3 from {2, 3, 4} yields {2, 4}.
-- Delete function
del :: Eq a => a -> Bag a -> Bag a
del _ [] = []  -- If the bag is empty, return an empty bag
del x ((y, count):ys)
  | x == y = if count > 1 then (y, count - 1) : ys else ys  -- If the element is found, decrement the count or remove if count is 1
  | otherwise = (y, count) : del x ys  -- Otherwise, continue searching in the rest of the bag
  
-- (c) Define a function bag that takes a list of values and produces a multiset representation.
bag :: Eq a => [a] -> Bag a
bag xs = foldl (flip ins) [] xs

-- (d) Define a function subbag that determines whether or not its first argument bag is contained in the second.
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag _ [] = False
subbag ((x, count_x): xs) ys = case lookup x ys of
    Just count_y -> count_x <= count_y && subbag xs ys
    Nothing -> False

 -- (e) Define a function isSet that tests whether a bag is actually a set, which is the case when each element occurs only once.
isSet :: Eq a => Bag a -> Bool
isSet = all (\(_, count) -> count == 1)

-- (f) Define a function size that computes the number of elements contained in a bag.
size :: Bag a -> Int
size = sum . map snd


---------------------------------------------------------------------------------------------------
-- Question 2 - Matrix

type Row    = [Int]
type Column = [Int]
type Matrix = [Row]

-- 2.a Vector Addition
vAdd :: Row -> Row -> Row
vAdd [] [] = []
vAdd (x:xs) (y:ys) = x + y : vAdd xs ys

vAddZipWith :: Row -> Row -> Row
vAddZipWith x y = zipWith (+) x y

-- 2.b Matrix Addition
mAdd :: Matrix -> Matrix -> Matrix
mAdd [] [] = []
mAdd x y = zipWith vAdd x y

-- 2.c Inner Product
iProd :: Row -> Column -> Int
iProd [] [] = 0
iProd (x:xs) (y:ys) = (x * y) + iProd xs ys

-- 2.d Outer Product
oProd :: Column -> Row -> Matrix
oProd [] _ = []
oProd (c:cs) ys = map (\x -> c * x ) ys : oProd cs ys

-- 2.e Size

mSize :: Matrix -> (Int, Int)
mSize [] = (0,0)
mSize (x:xs) = let (r, c) = mSize xs in (r + 1, foldl (\acc x -> acc + 1) 0 x)



---------------------------------------------------------------------------------------------------
-- Question 3 - Higher Order Funcs

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

main :: IO ()
main = do
    print("Question 1")
    let emptyBag = [] :: Bag Int  -- empty bag
    print("Empty ", emptyBag)

    putStrLn "\n(a) - Checks for insert"
    -- add a num into the empty bag
    let bag1 = ins 9 emptyBag  
    print("bag1 ", bag1)

    -- insert a different number twice, original again
    let bag2 = (ins 9 (ins 5 (ins 5 bag1)))
    print("bag2 ", bag2)

    putStrLn "\n(b) Checks for delete"
    let bag3 = del 5 bag2
    print("bag3 ", bag3)
    
    putStrLn "\n(c) - Checks for init"
    let xs = [7,3,8,7,3,2,7,5]
    let x_bag = bag xs
    print("x_bag ", x_bag)

    putStrLn "\n(d) - Checks for subbag"
    let x_big_list = [7,3,8,7,3,2,7,5,6,6,6]
    let x_big_bag = bag x_big_list
    let result = x_bag `subbag` x_big_bag
    print("x_bag subbag x_big_bag: ", result)
    let result = x_big_bag `subbag` x_bag
    print("x_big_bag subbag x_bag: ", result)

    putStrLn "\n(e) Checks for isSet"
    print("isSet emptyBag: ",  isSet( emptyBag ) )
    print("isSet bagWithOne: ",  isSet( bag1 ) )
    print("isSet x_bag: ", isSet( x_bag ) )

    putStrLn "\n(f) Checks for size"
    print("size emptyBag: ",  size( emptyBag ) )
    print("size bagWithOne: ",  size( bag1 ) )
    print("size x_bag: ", size( x_bag ) )

    print("Question 2")
    putStrLn "\n(a) - Vector Addition"
    let v1 = [ 1, 2, 3, 4]
    let v2 = [ 2, 4, 6, 8]
    let vA = vAdd v1 v2
    let vAZ = vAddZipWith v1 v2

    print("Recursive Version: ", vA)
    print("zipWith Version:   ", vAZ)

    putStrLn "\n(b) - Matrix Addition"
    let m1 = [[1,2,3],
              [4,5,6],
              [7,8,9]]
    let m2 = [[10,20,30],
              [40,50,60],
              [70,80,90]]
    let mA = mAdd m1 m2
    print("Added together: ", mA)

    putStrLn "\n(c) - Inner Product"
    let vI = iProd v1 v2
    print("Inner product: ", vI)

    putStrLn "\n(d) - Outer Product"
    let vO = oProd v1 v2
    print("Outer product: ", vO)

    putStrLn "\n(e) - Size"
    let vO12 = oProd v1 v2
    let s12 = mSize vO12
    print("Size of outer product: ", s12)

    let v3 = [1,2,3]
    let v4 = [4,5,6,7]
    let vO34 = oProd v3 v4
    let s34 = mSize vO34
    let vO43 = oProd v4 v3
    let s43 = mSize vO43
    print("Outer product: ", vO34)
    print("Size of outer product: ", s34)
    print("Outer product: ", vO43)
    print("Size of outer product: ", s43)