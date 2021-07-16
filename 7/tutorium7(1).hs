{-}
opNand :: Bool -> Bool -> Bool
opNand x y = (not x && not y)

sumofLists :: [Int] -> [Int] -> [Int]
sumofLists xs ys
  | xs == [] && ys == [] = []
  | xs == [] = ys
  | ys == [] = xs
  | otherwise = [head (xs) + head (ys)] ++ sumofLists (tail xs) (tail ys)

sumofLists2 :: [Int] -> [Int] -> [Int]
sumofLists2 xs ys
  |xs  == [] ys
  |ys == [] xs
sumofLists2 (x:xs) (y:ys) = (x + y): (sumLists xs ys)

--tutorium 19.12.2019

listenprodukt :: [Int] -> Int
listenprodukt xs = foldl (*) 1 xs

plusEins :: [Int] -> [Int]
plusEins xs = map (+1) xs

evenfactorials :: Int -> [Int]
evenfactorials n = [fac x | x <- [1..n], mod x 2 == 0]

twoTupel :: (Int,Int) -> (Int, Int) -> (Int, Int)
twoTupel (a,b) (x,y) = (a+x,b+y)

summeTupel :: [(Int,Int)] -> (Int,Int)
summeTupel (x:xs) = foldl(twoTupel) (0,0) (x:xs)

summeTupel2  :: [(Int,Int)] -> (Int,Int)
summeTupel2 xs = foldl twoTupel (0,0) xs

-}

data BinTree = Node BinTree BinTree |Leaf Int

sumOfLeafs :: BinTree -> Int
sumOfLeafs (Leaf n) = n
sumOfLeafs (Node t1 t2) = (sumOfLeafs t1) + (sumOfLeafs t2)

main = do
  print (sumOfLeafs (Node(Node(Leaf 5) (Leaf 6)) (Leaf 7)))
