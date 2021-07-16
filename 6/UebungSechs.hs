equalSums :: Int -> Int -> Int -> Int -> Bool
equalSums a b c d
  |a + b == c + d = True -- Fallunterscheidung für mögliche Summenpaare aus 4
                         -- Integer Eingaben. Falls die Summe der 2 Paare,
                         -- welche jeweils aus der Addition von 2 Integern
                         -- bestehen, gleich sind, dann ist der
                         -- Ausgabeparameter True. Die Integer kommen ohne
                         -- Widerholung vor, weshalb wir 6 verschidene
                         -- Summenpaare haben und 3 Fälle um zu untersuchen, ob
                         -- die Summe der Summenpaare gleich ist.
  |a + c == b + d = True
  |a + d == b + c = True
  |otherwise = False     -- Ist dies nicht der Fall, ist der Ausgabeparameter
                         -- False.

numberOfPwd :: Int -> Int -> Int -> Int
numberOfPwd a b c
  |a == b && b == c = 0  -- Sind die 3 Eingabeparametere gleich, dann gibt es
                         -- keine paarweise unterschiedliche Werte, weshalb der
                         -- Ausgabeparameter 0 ist.
  |a /= b && b /= c = 3  -- Sind alle 3 Eingabeparameter unterschiedlich, dann
                         -- ist der Ausgabeparameter 3, da es 3
                         -- paarweise unterschiedliche Werte gibt.
  |otherwise = 2         -- Der dritte Fall, beschreibt die Möglichkeit, dass 2
                         -- der Eingabeparameter gleich sind und der dritte
                         -- einen anderen Wert hat. Es gibt daher 2 paarweise
                         -- verschiedene Werte und der Ausgabeparameter ist in
                         -- dem Fall 2.

width :: Float -> Float -> Float -> Float
width a b c
  |a >= b && b >= c = a - c -- Man muss bei der Funktion zwischen 6 Fällen
                          -- unterscheiden. In diesem Fall ist der
                          -- Eingabeparameter a größer als b und b ist größer
                          -- als c. Um die Differenz des größten und kleinsten
                          -- Wertes auszugeben, muss man nun c von a
                          -- subtrahieren.
  |a >= c && c >= b = a - b -- In diesem Fall ist a größer als c und c ist größer
                          -- als b, weshalb man b von a subtrahiert.
  |b >= a && a >= c = b - c -- In diesem Fall ist b größer als a und a ist größer
                          -- als c, weshalb man c von b subtrahiert.
  |b >= c && c >= a = b - a -- In diesem Fall ist b größer als c und c ist größer
                          -- als a, weshalb man a von b subtrahiert.
  |c >= b && b >= a = c - a -- In diesem Fall ist c größer als b und b ist größer
                          -- als a, weshalb man a von c subtrahiert.
  |c >= a && a >= b = c - b -- In diesem Fall ist c größer als a und a ist größer
                          -- als b, weshalb man b von c subtrahiert.

missing :: Int -> Int -> Int -> Int
missing a b c
  | a /= b && b /= c = set1234 a b c          -- Die Eingabeparameter a,b und c
                                              -- sind verschieden und aus der
                                              -- Menge {1,2,3,4}.
     where                                    -- Ich definiere hier eine Hifs-
                                              -- funktion.
         set1234 :: Int -> Int -> Int -> Int
         set1234 a b c
             | a + b + c == 6 = 4             -- Sind a,b und c gleich 1, 2 und
                                              -- 3 und addiert man diese
                                              -- Integer erhält man 6 und da die
                                              -- Addition von 1, 2 und 3 die
                                              -- einzige Möglichkeit ist aus 3
                                              -- Werten aus der Menge die Zahl 6
                                              -- zu erhalten, kann man daraus
                                              -- schließen, dass der fehlende
                                              -- Wert, der ausgegeben werden
                                              -- soll 4 ist.
             | a + b + c == 7 = 3             -- Sind a, b und c gleich 1,
                                              -- 2 und 4 erhält man bei der
                                              -- Addition der 3 Zahlen 7 und
                                              -- kann daraus schließen, dass der
                                              -- fehlende Wert 3 ist.
             | a + b + c == 8 = 2             -- Sind a, b und c gleich 1,
                                              -- 3 und 4 erhält man bei der
                                              -- Addition der 3 Integer 8 und
                                              -- kann daraus schließen, dass
                                              -- der fehlende Wert 2 ist.
             | a + b + c == 9 = 1             -- Sind a, b und c gleich 2, 3
                                              -- und 4 erhält man bei der
                                              -- Addition der 3 Integer 9 und
                                              -- kann daraus schließen, dass
                                              -- der fehlende Wert 1 ist.
             |otherwise = 0                   -- Trifft keiner der 4 Fälle zu,
                                              -- hat die Funktion den Ausgabe-
                                              -- parameter 0.

valueAt :: Float -> Float -> Float -> Float
valueAt x y z = (y / (-x)) * z + y  -- Die allgemeine Gleichung einer Gerade ist
                                    -- y = m*x + n. Die Steigung m errechnen wir
                                    -- indem wir Die Differenz von y2 und y1
                                    -- ausrechnen und diese Differenz durch die
                                    -- Differenz von x2 und x1 teilen. Y2 ist
                                    -- ein Eingabeparameter, Y1 ist 0, da ein
                                    -- Schnitt mit der X-Achse vorliegt. X2 ist
                                    -- 0, da die X-Achse geschnitten wird und X1
                                    -- ist ein Eingabeparameter. n ist in
                                    -- diesem Fall Y2, da n den Schnitt mit der
                                    -- Y-Achse darstellt. Gibt man nun einen
                                    -- weiteren Wert x ein, erhält man den Wert
                                    -- y, so dass der Punkt (x,y) auf g liegt.

testParallel :: Float -> Float -> Float -> Float -> Bool
testParallel x y a b
  |(y / (-x)) == (b / (-a)) =True  -- Damit eine 2 Geraden Parallel zueinander
                                   -- sind, müssen die Steigungen gleich sein.
                                   -- Die Berechnung der Steigung ist bereits
                                   -- in 2a) beschrieben worden. Sind die
                                   -- Steigung der Geraden g und h gleich, dann
                                   -- ist der Ausgabeparameter True.
  |otherwise = False               -- Sind die Steigungen unterschiedlich, ist
                                   -- der Ausgabeparameter False.

parallelThroughX :: Float -> Float -> Float -> Float
parallelThroughX x y a = (-a) * (y / (-x)) -- Damit die Geraden parallel zu-
                                           -- einander sind, müssen beide
                                           -- Steigungen wieder gleich sein.
                                           -- Aus den Eigabeparametern
                                           -- errechnet man die Steigung für
                                           -- die Gerade g. Des Weiteren haben
                                           -- wir noch den Eingabeparamter a,
                                           -- welcher die Differenz zwischen
                                           -- X2 und X1 darstellt. Aus den 3
                                           -- Eingabeparametern soll die
                                           -- Funktion den Wert y so bestimmen,
                                           -- dass die Steigungen beider Geraden
                                           -- gleich sind. Man stellt die
                                           -- Gleichung nun so um, dass die
                                           -- Funktion parallelThroughX den
                                           -- Wert y berechnet.

crossingAt :: Float -> Float -> Float -> Float -> Float
crossingAt xg yg xh yh
  | yg / (-xg) /= yh / (-xh) = (yh - yg) / (yg/(-xg) - yh/(-xh)) -- Die Steigung
                                                    -- mg und mh sollen nicht
                                                    -- gleich sein. Man setzt
                                                    -- beide Geradengleichungen
                                                    -- gleich, und stellt die
                                                    -- Gleichung nach dem
                                                    -- gesuchten x-Wert um, der
                                                    -- dann die x-Koordinate
                                                    -- ausgibt.
  | otherwise = error "error"                       -- Sind die Steigungen
                                                    -- gleich, gibt die Funktion
                                                    -- error aus.
