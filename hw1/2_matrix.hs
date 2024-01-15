type Row    = [Int]
type Column = [Int]
type Matrix = [Row]


vAdd :: Row -> Row -> Row
vAdd [] [] = []
vAdd (x:xs) (y:ys) = x + y : vAdd xs ys

main :: IO ()
main = do
    putStrLn "\n(a) - Vector Addition"
    -- add a num into the empty bag
    let v1 = [ 1, 2, 3, 4]
    let v2 = [ 2, 4, 6, 8]
    let added = vAdd v1 v2

    print("Added together: ", added)

 
    putStrLn "\nDONE"