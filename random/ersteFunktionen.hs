lieblingszahl :: Int -> Bool
lieblingszahl 12 = True
lieblingszahl x = False
my_add :: Int -> Int -> Int
my_add x y = x + y
my_substract :: Int ->Int -> Int
my_substract x y = x - y
my_multiply :: Int -> Int -> Int
my_multiply x y = x * y
my_not :: Bool -> Bool
my_not True = False
my_not _ = True
decide_mul_div :: Bool -> Float -> Float
decide_mul_div True x = x * 2
decide_mul_div _ x = x / 2