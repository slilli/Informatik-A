countDigits :: String -> Int
countDigits [] = 0
countDigits (ch:chs)
  | ch >= '0' && ch <= '9' = 1 + countDigits (chs)
  | otherwise = countDigits (chs)

listOfDiffs :: [Int] -> [Int]
listOfDiffs [] = []
listOfDiffs [x] = []
listOfDiffs (y:x:xs) = (y-x) : listOfDiffs (x:xs)

maxOfLists :: ([Int], [Int]) -> [Int]
maxOfLists ([],[]) = []
maxOfLists (xs,[]) = xs
maxOfLists ([],ys) = ys
maxOfLists (x:xs,y:ys)
  |x >= y = (x) : maxOfLists (xs,ys)
  |y >= x = (y) : maxOfLists (xs,ys)
