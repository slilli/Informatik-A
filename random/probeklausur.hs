data Salzwasser = Salz Float | Wasser Float | Mix Salzwasser Salzwasser

salz, wasser, ges :: Salzwasser -> Float

salz (Salz x) = x
salz (Wasser x) = 0
salz (Mix sw1 sw2) = salz sw1 + salz sw2

wasser (Salz x) = 0
wasser (Wasser x) = x
wasser (Mix sw1 sw2) = wasser sw1 + wasser sw2

ges sw = salz sw + wasser sw

instance Eq Salzwasser where
  sw1 == sw2 = (salz sw1 == salz sw2) && (wasser sw1 == wasser sw2)

instance Show Salzwasser where
  show sw = show (ges sw) ++ "g Salzwasser mit einem Salzgehalt von" ++ show ((salz sw) / (ges sw)) ++ "Prozent"
