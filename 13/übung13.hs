data CodeTree = Leaf Char | Node CodeTree CodeTree

--a)
valid::CodeTree->Bool
valid t = v (toString t)
    where
    v::String->Bool
    v [x] = True
    v (x:xs)
        |elem x xs = False
        |otherwise = v (xs)

makeTable::CodeTree->[(Char,[Int])]
makeTable (Leaf a) = [(a,[])]
makeTable tree = mT (toString tree) tree
    where
    mT::String->CodeTree->[(Char,[Int])]
    mT [] tree = []
    mT (x:xs) tree = (x, (charCode x tree)) : mT xs tree

--b)
charCode :: Char -> CodeTree -> [Int]
charCode ch (Leaf a) = []
charCode ch (Node n0 n1)
    | elem ch (toString n0) = 0 : charCode ch n0
    | elem ch (toString n1) = 1 : charCode ch n1
    | otherwise = error "meh"


toString :: CodeTree -> String
toString (Leaf a)     = [a]
toString (Node t0 t1) = toString t0 ++ toString t1


codeStr::[Char]->CodeTree->[Int]
codeStr [] tree = []
codeStr (x:xs) tree
    |valid tree == False = error "nope"
    |elem x (toString tree) == False = error "not today"
    |valid tree && elem x (toString tree) = (charCode x tree) ++ (codeStr xs tree)


baum1 = (Node (Node (Leaf 'a')(Leaf 'c'))(Node (Node (Leaf 'f') (Leaf 'd'))(Node (Leaf 'b')(Leaf 'e'))))


baum::CodeTree
baum = (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
