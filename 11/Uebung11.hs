data Tree a = T a [Tree a]

--Aufgabe 1
--a)
size :: Tree a -> Int
size (T a []) = 1
size (T a (x:xs)) = size x + size (T a xs)

height :: Tree a -> Int
height (T a []) = 0
height (T a (x:xs)) = 1 + max (height (T a xs)) (height (T a xs))

--b)
loadOfInner :: Tree Int -> Int
loadOfInner (T n []) = 0
loadOfInner (T n xs) = foldl (a) n xs
  where
    a :: Int -> Tree Int -> Int
    a i (T n []) = i + n
    a i (T n (y:[ys])) = a (i + n) ys

maxLoadPath :: Tree Int -> Int
maxLoadPath (T a [])
maxLoadPath (T a xs) =

main = do
  putStrLn "junge wie heisst du "
  name <-getLine
  putStrLn ("du heisst " ++ name ++ "Huso")

--c)
preOrderTrav :: Tree a -> [a]
preOrderTrav (T a []) = []
preorderTrav (T a [x]) = (x:xs) ++ preorderTrav a ++ preOrderTrav [x]


{-
data Tree a = T a [Tree a]
Tree Int
funktion :: Tree Int -> Int
funktion (T a xs)
main = do
  print (funktion (T 7 [(T 3 [], (T 4 [], (T 5 [])])) -- wird so noch nicht funktionieren
-}
