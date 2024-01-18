import GHC.Exts.Heap (GenClosure(key))
import Text.Parsec.Error (Message(SysUnExpect))

type Points = Int

data Grade = Pass | Fail deriving Show
data Bigness = Bigger | Smaller deriving (Show,Eq)
alice :: Points
alice = 65

grade :: Points -> Grade
grade p = if p>50 then Pass else Fail

type Bag a = [(a, Int)]

-- Each element x occurs in at most one pair in the list
-- Each element that occurs in a pair has a positive counter.

-- let x = [(5,1),(7,3),(2,1),(3,2),(8,1)]

whatBigger :: (Int, Int) -> Bigness
whatBigger (j, k) = if j > k then Bigger else Smaller

-- rev :: [Int] -> [Int]
-- rev [] = []
-- rev (x:xs) = rev xs ++ [x]

-- doubleRev :: [Int] -> [Int]
-- doubleRev [] = []
-- doubleRev (x:xs) = doubleRev xs ++ [x] ++ [x]

incList :: [Int] -> [Int]
--incList [] = []
--incList (x:xs) = [x + 1] ++ incList xs
--incList (x:xs) = (x+1) : incList xs
incList = map (+1)
(++) :: [Int] -> [Int] -> [Int]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs Main.++ ys)

age :: Int
age = 25


firstGTFive :: (Ord a, Num a) => [a] -> Bool
firstGTFive (x:xs) = x > 5
firstGTFive [] = False
