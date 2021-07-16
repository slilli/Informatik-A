{-
primesR :: Integral a => a -> a -> [a]
primesR a b = takeWhile (<= b) $ dropWhile (< a) $ sieve  [2..]
  where sieve (n:ns) = n : sieve [m | m <- ns, m `mod` n /= 0]

sieve :: [Int] -> [Int]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]

import Data.Char

g x y z = [ (s,t) | s <- y, t <- z, s < x, x < t ]

data Baum = Nil | Knoten Baum Int Baum

evenToList :: Baum -> [Int]
evenToList Nil = []
evenToList (Knoten lub x rub)
  | even x = evenToList lub ++ evenToList rub ++ [x]
  | otherwise = evenToList lub ++ evenToList rub

evenToList1 :: Baum -> [Int]
evenToList1 = filter even.preorder
-}
import Data.Char

f x y z = map fst [ (s,t) | s <- y, t <- z, mod s t == ord x ]
