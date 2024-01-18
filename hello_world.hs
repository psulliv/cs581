b :: Int -> Int -> [Int]
b y x  = [ x, y, x]

main = do
  putStrLn "this is output"
  a <- getLine
  let aInt = read a :: Int
  print (b aInt aInt)
