delReplic1 :: String -> String
delReplic1 [] = []
delReplic1 (x:xs)
  |elem x xs = delReplic1 xs
  |otherwise = x : (delReplic1 xs)

loeschen :: Char -> String -> String
loeschen a [] = []
loeschen a (x:xs)
  |a == x = loeschen a xs
  |otherwise = x : (loeschen a xs)

delReplic2 :: String -> String
delReplic2 [] = []
delReplic2 (x:xs) = x : (delReplic2 (loeschen x xs))

sumWithSigns :: [(Float,Bool)] -> Float
sumWithSigns [] = 0
sumWIthSIgns ((x, bool) : xs)
  |bool = x + sumWIthSigns (xs)
  |otherwise = (-x) + sumWIthSigns (xs)

sumOfTwo :: [Float] -> [Float] -> [Float]
sumOfTwo xs ys
  |xs == [] = ys
  |ys == [] = xs
sumOfTwo (x:xs) (y:ys) = (x+y) : (sumOfTwo xs ys)

sumOfAll :: [[Float]] -> [Float]
sumOfAll [] = []
sumOfAll [x] = x
sumOfAll (x:y:ys) = sumOfAll ((sumOfTwo x y) :ys)

av1 :: [Float] -> Float
av1 xs = (/) (sum xs) (fromIntegral (length xs))

av2 :: [Float] -> Float -> [Float]
av2 [] n = []
av2 (x:xs) n = (x/n) : (av2 xs n)

average1 :: [[Float]] -> [Float]
average1 [] = []
average1 (x:xs) = (av1 x) : (average1 xs)

average2 :: [[Float]] -> [Float]
average2 xs = av2 (sumOfAll xs) (fromIntegral (length xs))

f :: Int -> [(Int,Int)]
f n = [(x,y) | x<- [1..n], y<-[1..n], mod (x*y) n == 0, y>=x]

part4 :: Int -> [(Int, Int, Int, Int)]
part4 n = [(w,x,y,z) | x<-[1..n], x<-[1..n], y<-[1..n], z<-[1..n], w+x+y+z == n]

add2:: Float -> (Float,Bool) -> Float
add2 x (y, nosign)
  |nosign = x+y
  |otherwise = x-y

newSumWIthSigns :: [(Float, Bool)] -> Float
newSumWithSigns xs = foldl add2 0 xs

newSumOfAll :: [[Float]] -> [Float]
newSumOfAll xs = foldr sumOfTwo [] xs

newAverage1 :: [[Float]] -> [Float]
newAverage1 xs = map av1 xs
