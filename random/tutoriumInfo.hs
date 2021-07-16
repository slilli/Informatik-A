import Data.Char

foo1 x y = x ++ [y:z | z <- x]

foo2 (x,y,z) = map y [s | s <- z, ord x <= s]

foo3 x y z
  | x == y = show z
  | otherwise = " "

foo4 f z (x:xs) = foldl f (f z x) xs

-- in ghci testen mit :type _____Funktionsname____
