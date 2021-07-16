import Data.Char

--Aufgabe 4
countDigits :: String -> Int
countDigits [] = 0
countDigits (ch:chs)
  | ch >= '0' && ch <= '9' = 1 + countDigits (chs) --laut ASCII Code Buchstabe
  | otherwise = 0 + countDigits (chs)

listOfDiffs :: [Int]-> [Int]
listOfDiffs [] = []
listOfDiffs [x] = []
listOfDiffs (y:x:xs) = (y-x) : listOfDiffs (x:xs)

maxOfLists :: ([Int],[Int]) -> [Int]
maxOfLists ([],[]) = []
maxOfLists (xs,[]) = xs
maxOfLists ([],ys) = ys
maxOfLists ((x:xs),(y:ys))
  | x >= y = (x) : maxOfLists (xs,ys)
  | y >= x = (y) : maxOfLists (xs,ys)

--Aufgabe 2

first :: (a,b,c) -> a  -- Hilfsfunktion: erste Variable aus Tripel
first (x,y,z) = x

gStep :: (Int,Int,Int) -> (Int,Int,Int)
gStep (x,y,z) = (y,z,z-2*y+x)  -- Nachfolger-Tripel: letzte Variable mithilfe
                               -- mithilfe g(n-1)-2g(n-2)+g(n-3) berechnen

gTripel :: Int -> (Int,Int,Int)  --Tripel von Eingabewert berechnen: gStep wird
                                 --so oft ausgeführt bis n=0
gTripel n
  | n == 0 = (1,2,3)
  | otherwise = gStep (gTripel (n-1))

fastg :: Int -> Int                 --erste Variable des berechneten Tripels
fastg n = first (gTripel n)

--Aufgabe 3

type Position = (Float,Float)
type Strike = (Float, Float)

hitsBank :: Position -> Strike -> Bool
hitsBank (x,y) (s,t)
  | x < 0 || x > 20 || y < 0 || y > 10 = error "Kugel liegt außerhalb des Feldes"
  | s == 0 || t == 0 = error "keine Horizontalen/Vertikalen Stöße erlaubt"
  | x + s < 0 || y + t < 0 = True --Kugel trifft Bande
  | x + s > 20 || y + t > 10 = True -- Kufel trifft Bande
  | otherwise = False

bankPos :: Position -> Strike -> Position
bankPos (x,y) (s,t)
  |(s >= 0) && (t >= 0) && ((s/t) >= ((10-y)/(20-x))) = (x + (((10-y)/t) * s), 10)
  |(s >= 0) && (t >= 0) && ((s/t) <= ((10-y)/(20-x))) = (20, y + (((20-x)/s) * t))
  |(s >= 0) && (t <= 0) && ((s/t) >= ((-y)/(20-x))) = (x + (((-y)/t)*s), 0)
  |(s >= 0) && (t <= 0) && ((s/t) <= ((-y)/(20-x))) = (20, y + (((20-x)/s)*t))
  |(s <= 0) && (t <= 0) && ((s/t) <= (y/x)) = (x + (((-y)/t)*s),0)
  |(s <= 0) && (t <= 0) && ((s/t) >= (y/x)) = (0,y + (((-x)/s)*t))
  |(s <= 0) && (t >= 0) && ((s/t) >= ((10-y)/(-x))) = (0, y + (((-x)/s)*t))
  |(s <= 0) && (t >= 0) && ((s/t) <= ((10-y)/(-x))) = (x + (((10-y)/t)*s),10)
--Begründung siehe handschriftliches Blatt

reflectHor :: Position -> Strike -> Strike
reflectHor (x,y) (s,t)
  | snd(bankPos (x,y) (s,t)) == 0 = ((s - (fst(bankPos (x,y) (s,t)) - x )),(-(t - (snd(bankPos (x,y) (s,t))-y))))
  | snd(bankPos (x,y) (s,t)) == 10 = ((s - (fst(bankPos (x,y) (s,t)) - x )),(-(t - (snd(bankPos (x,y) (s,t))-y))))
  | otherwise = error "Manno"
--Ist der y Wert von bankPos gleich 0 oder 10, dann reflektiert die Kugel an der
--Horizontalen Bande.
--Bei Berechnung des Reflektionsparameters wird der Reflektionspunkt an der Bande
--als neuer Punkt benutzt um den Rückstoßparameter auszurechnen

reflectVert :: Position -> Strike -> Strike
reflectVert (x,y) (s,t)
  | fst(bankPos (x,y) (s,t)) == 20 = (-(s - (fst (bankPos (x,y) (s,t)) -x)),((t - (snd (bankPos (x,y) (s,t)) -y))))
  | fst(bankPos (x,y) (s,t)) == 0 = (-(s - (fst (bankPos (x,y) (s,t)) -x)),((t - (snd (bankPos (x,y) (s,t)) -y))))
  | otherwise = error "Manno"
--Ist der x Wert von banPos gleich 0 oder 20, dann reflektiert die Kugel an der
--Vertikalen Bande.

executeStrike :: Position -> Strike -> Position
executeStrike (x,y) (s,t)
  | x > 20 || y > 10 = error "Die Kugel sollte schon auf dem Tisch liegen"
  | s == 0 || t == 0 = error "vert und hor Stöße sind nicht erlaubt"
  | hitsBank (x,y) (s,t) == False = ((x+s),(y+t))
  | hitsBank (x,y) (s,t) == True && snd(bankPos(x,y) (s,t)) == 10 = (fst(bankPos (x,y)(s,t)) + (fst(reflectHor (x,y) (s,t))) ,((snd(bankPos (x,y) (s,t))) + (snd(reflectHor (x,y) (s,t)))))
  | hitsBank (x,y) (s,t) == True && snd(bankPos(x,y) (s,t)) == 0 = (fst(bankPos (x,y)(s,t)) + (fst(reflectHor (x,y) (s,t))) ,((snd(bankPos (x,y) (s,t))) + (snd(reflectHor (x,y) (s,t)))))
  | hitsBank (x,y) (s,t) == True && fst(bankPos(x,y) (s,t)) == 20 = ((fst(bankPos (x,y)(s,t))) + (fst(reflectVert (x,y) (s,t))) ,((snd(bankPos (x,y) (s,t))) + (snd(reflectVert (x,y) (s,t)))))
  | hitsBank (x,y) (s,t) == True && fst(bankPos(x,y) (s,t)) == 0 = ((fst(bankPos (x,y)(s,t))) + (fst(reflectVert (x,y) (s,t))) ,((snd(bankPos (x,y) (s,t))) + (snd(reflectVert (x,y) (s,t)))))
  | hitsBank (x,y) (s,t) == True && bankPos (x,y) (s,t) == (0,10) = (((-s)+fst(bankPos (x,y) (s,t))-x),((-t)+ snd(bankPos (x,y) (s,t))-y))
  | hitsBank (x,y) (s,t) == True && bankPos (x,y) (s,t) == (0,0) = (((-s)+fst(bankPos (x,y) (s,t))-x),((-t)+ snd(bankPos (x,y) (s,t))-y))
  | hitsBank (x,y) (s,t) == True && bankPos (x,y) (s,t) == (20,0) = (((-s)+fst(bankPos (x,y) (s,t))-x),((-t)+ snd(bankPos (x,y) (s,t))-y))
  | hitsBank (x,y) (s,t) == True && bankPos (x,y) (s,t) == (20,10) = (((-s)+fst(bankPos (x,y) (s,t))-x),((-t)+ snd(bankPos (x,y) (s,t))-y))
--Begründung siehe handschriftliches Blatt
