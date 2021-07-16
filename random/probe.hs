data CTree = Leaf Char | Node CTree CTree
--Aufgabe 3
--a)
h :: CTree -> Int
h (Leaf n) = 0
h (Node a b) = 1 + max (h a) (h b)

longest :: CTree -> Char
longest (Leaf n) = n
longest (Node (Leaf a) (Leaf b)) = a
longest (Node a b)
  |h a >= h b = longest a
  |h b <= h a = longest b

--b)
codeList :: CTree -> [Char]
codeList (Leaf n) = [n]
codeList (Node a b) = codeList a ++ codeList b


checkSymb :: Char -> CTree -> Bool
checkSymb a (Leaf ch)
  |a == ch = True
  |a /= ch = False
checkSymb d (Node c1 c2)
  |elem d (codeList c1) || elem d (codeList c2) = True
  |otherwise = False


checksy ::Char -> CTree -> Bool
checksy a xs = elem a (codeList xs)
{--
correct :: CTree -> Bool
correct xs = correct2 (codeList xs)
   where
     correct2 :: [Char] -> Bool
     correct2 [] = True
     correct (x:xs) = not (elem x:xs) && correct2 xs
--}
correct2::CTree->Bool
correct2 (Leaf ch) = True
correct2 t = help (codeList t)
   where
    help::String->Bool
    help [x] = True
    help (x:xs)
        |elem x xs = False
        |otherwise = help xs

--c)
code :: CTree -> Char -> [Int]
code (Leaf n) d = []
code (Node c1 c2) d
  |checkSymb d (Node c1 c2) = leftOrRight d (Node c1 c2)
  |otherwise = [-1]
        where
          leftOrRight d (Leaf n) = []
          leftOrRight d (Node c1 c2)
            |elem d (codeList c1) = 0 : leftOrRight d c1
            |elem d (codeList c2) = 1 : leftOrRight d c2

code2 :: CTree -> Char ->[Int]
code2 (Leaf a) b
  | a == b = []
  | otherwise = [-1]
code2 ( Node a b) c
  | checkSymb c a = [0] ++ (code a c)
  | checkSymb c b = [1] ++ (code b c)
  | otherwise = [-1]

--d)

equalcoded2 :: CTree -> CTree -> [Char]
equalcoded2 (Leaf a) (Leaf b)
   | a == b = [a]
   | otherwise = []
equalcoded2 (Node a b) (Node c d) = (equalcoded2 a c) ++ (equalcoded2 b d)
equalcoded2 _ _ = []

baum :: CTree
baum = (Node (Node (Leaf 'a') (Leaf 'c')) (Node (Node (Leaf 'f') (Leaf 'd')) (Node (Leaf 'b') (Leaf 'e'))))

--Aufgabe 4
--a)
f::Eq a=>[a]->[a]
f [] = []
f [x] = [x]
f (x:xs)
    |elem x xs = f (xs)
    |otherwise = x : f (xs)

--b)
threePart::Int->[(Int,Int,Int)]
threePart n = [(a,b,c) | a<-[1..n], b<-[1..n], c<-[1..n], (a+b+c)==n ]

--threePartz::Int->[(Int,Int,Int)]
--threePartz n = [(a,b,c) | a<-[1..n], b<-[1..n], n-(a+b)==c , (a+b+c)==n
