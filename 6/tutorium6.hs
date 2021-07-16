inL2 :: String -> Bool
inL2 a
  |a == "" = False
  |a == "a" = True
  |(head a) == 'a' && (last a) == 'b' = inL2 (tail(init a))
  |(head a) == 'a' = inL2 (tail a)
  |otherwise = False

inL1 :: String -> Bool
inL1 a
  |a == "" = True
  |a == "a" = True
  |a == "b" = True
  |(head a) == 'a' && (last a) == 'a' = inL1 (tail(init a))
  |(head a) == 'b' && (last a) == 'b' = inL1 (tail(init a))
  |otherwise = False