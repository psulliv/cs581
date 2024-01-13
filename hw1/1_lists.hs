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

main :: IO ()
main = do
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

    putStrLn "\nDONE"