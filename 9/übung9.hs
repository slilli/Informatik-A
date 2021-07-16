import Data.Char
import Data.List

--Aufgabe 1
delReplic1::String->String
delReplic1 [] = []
delReplic1 [ch] = [ch]
delReplic1 (ch:chs)
    |elem ch chs == True = delReplic1 chs
    |otherwise = ch : delReplic1 chs


delReplic2::String->String
delReplic2 [] = []
delReplic2 (ch:chs)
    |elem ch chs == True = ch : delete2 ch (delReplic2 chs)
    |otherwise = ch : delete2 ch (ch:chs)

delete2::Char->String->String
delete2 ch [] = []
delete2 ch (x:xs)
    |elem ch [x] == True && elem ch xs == True = delete2 ch xs
    |elem ch [x] == True && elem ch xs == False = delete2 ch xs
    |elem ch [x] == False && elem ch xs == True = x:(delete2 ch xs)
    |otherwise = x:(xs)


sumWithSigns::[(Float,Bool)]->Float
sumWithSigns [] = 0
sumWithSigns (x:xs)
    |snd x == True = fst x + sumWithSigns xs
    |snd x == False = -(fst x) + sumWithSigns xs
    |otherwise = error "Orischwerblöde"


sumOfTwo::[Float]->[Float]->[Float]
sumOfTwo xs ys
    |xs == [] = ys
    |ys == [] = xs
sumOfTwo (x:xs) (y:ys) = (x+y) : sumOfTwo xs ys

sumOfAll::[[Float]]->[Float]
sumOfAll [[]] = []
sumOfAll [(x:xs)] = (x:xs)
sumOfAll (y:x:xs) = sumOfAll ((sumOfTwo y x) : xs)


average1:: [[Float]]->[Float]
average1 [] = []
average1 [(x:xs)] = [sum xs / lengtha xs]
average1 (x:xs) = sum x / lengtha x : average1 xs

lengtha :: [a] -> Float
lengtha [ ] = 0
lengtha (x:xs) = 1 + lengtha xs

average2:: [[Float]]->[Float]
average2 [] = []
average2 [[x]] = [x]
average2 (y:ys) = helpme (sumOfAll (y:ys))

    where
     helpme::[Float]->[Float]
     helpme [] = []
     helpme [x] = [x/lengtha (y:ys)]
     helpme (x:xs) = (x/lengtha (y:ys)) : (helpme xs)



--Aufgabe 2

f::Int->[(Int,Int)]
f n = [ (a,b) | a <- [1..n], b <- [a..n], mod (a * b) n == 0]

part4::Int->[(Int,Int,Int,Int)]
part4 n = [ (a,b,c,d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], a+b+c+d == n ]


 --Aufgabe 3 : je einmal rechts- und linksfaltung-}

newSumWithSigns::[(Float,Bool)]->Float
newSumWithSigns [] = 0
newSumWithSigns (x:xs) = foldl (+) 0 [helpme2 (x)] + (newSumWithSigns xs)
    where
    helpme2::(Float,Bool) -> Float
    helpme2 (a,b)
        |b == True = a
        |b == False = -(a)

newSumOfAll::[[Float]]->[Float]
newSumOfAll (x:xs) = foldl (sumOfTwo) x xs


newAverage1::[[Float]]->[Float]
newAverage1 (x:xs) = map helpme3 (x:xs)
    where
    helpme3::[Float]->Float
    helpme3 (x) = sum (x) / lengtha (x)
