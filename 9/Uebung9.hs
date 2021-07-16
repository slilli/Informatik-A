import Data.Char


delReplic1 :: String -> String
delReplic1 [] = []
delReplic1 (x:xs)
  |x `elem` xs = delReplic1 xs
  |otherwise = x : delReplic1 xs

delReplic2 :: String -> String
delReplic2 [] = []
delReplic2 (ch:chs)
  | elem ch chs == True = ch : delete2 ch (delReplic2 chs)
  | otherwise = ch : delete2 ch (ch:chs)

delete2 :: Char -> String -> String
delete2 ch [] = []
delete2 ch (x:xs)
  | elem ch [x] == True && elem ch xs == True = delete2 ch xs
  | elem ch [x] == True && elem ch xs == False = delete2 ch xs
  | elem ch [x] == False && elem ch xs == True = x:(delete2 ch xs)
  | otherwise = x:(xs)

sumWithSigns :: [(Float,Bool)] -> Float
sumWithSigns [] = 0
sumWithSigns (x:xs)
  |snd x == True = (fst x) + sumWithSigns xs
  |snd x == False = -(fst x) + sumWithSigns xs
  |otherwise = error "MANNO"
{-}
sumOfTwo :: [Float] -> [Float] -> [Float]
sumOfTwo xs ys
  |xs  == [] = ys
  |ys == [] = xs
sumOfTwo (x:xs) (y:ys) = (x + y): (sumOfTwo xs ys)

sumOfAll :: [Float] -> [Float] -> [Float]
sumOfAll [[]] = []
sumOfAll [(x:xs)] = (x:xs)
sumOfAll (y:x:xs) = sumOfAll ((sumOfTwo y x) : xs)

average1 :: [[Float]] -> [Float]
average1 [] = []
average1 [(x:xs)] = [sum xs / lengths xs]
average1 (x:xs) = sum x / lengths x : average1 xs

lengths :: [a] -> Float
lengths [] = 0
lengths (x:xs) = 1 + lenghts xs

average2 :: [[Float]] -> [Float]
average2 [] = []
average2 [[y]] = [y]
average2 (y:ys) = helpme (sumOfAll (y:ys))
  where
    helpme :: [Float] -> [Float]
    helpme [] =[]
    helpme [x] = [x / lengths (y:ys)]
    helpme (x:xs) = (x / lengths (y:ys)) : (helpme xs)
-}
f :: Int -> [(Int,Int)]
f n = [ (a,b) | a <-[1..n], b <-[a..n]]

part4 :: Int -> [(Int,Int,Int,Int)]
part4 n = [ (a,b,c,d) | a <-[1..n], b <-[1..n], c <-[1..n], d <-[1..n], a+b+c+d == n]

newSumWithSigns :: [(Float,Bool)] -> Float
newSumWithSigns [] = 0
newSumWithSigns ((a,b):xs) = foldl (+) 0 [helpme2 (a,b)] + newSumWithSigns xs
  where
    helpme2 :: (Float,Bool) -> Float
    helpme2 (a,b)
      |b == True = (a)
      |b == False = -(a)

{-
newSumOfAll :: [[Float]] -> [Float]
newSumOfAll (x:xs) = foldl (sumOfTwo x) xs

newAverage1 :: [[Float]] -> [Float]
newAverage1 (x:xs) = map (lengths x/) sum x
-}
