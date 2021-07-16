a :: Int -> Int
a x = mod x 2

b :: Int -> Int
b x = div x 2

c :: Int -> Int -> String
c x y = show (x) ++ "+"++ show(y) ++ "ist" ++ show(x+y)

d:: Int -> Int
d x
  |x < 0 = -d(-x)
  |x == 0 = 0
  |otherwise = 2+ d (x-1)

kGauss :: Int -> Int
kGauss x
  |x == 0 = 0
  |otherwise = x + kGauss(x-1)

Gauss :: Int -> Int
Gauss x
  |x>0 = x + Gauss (x-1)
  |otherwise = 0
  