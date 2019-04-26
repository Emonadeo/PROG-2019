-- Übung 03 - Programmierung TU Dresden
-- Tutor: Eric Kunze
-- Github: https://github.com/oakoneric/programmierung-ss19

data Tree = Node Int Tree Tree | NIL

tree1 :: Tree
tree1 = Node 5 (Node 3 NIL NIL) (Node 6 NIL NIL)

-- Aufgabe 01A
insert :: Tree -> [Int] -> Tree
insert t [] = t
insert NIL (x:xs) = insert (Node x NIL NIL) xs
insert t@(Node n t1 t2) (x:xs)
    | x < n = insert (Node n (insert t1 [x]) t2) xs
    | x > n = insert (Node n t1 (insert t2 [x])) xs
    | otherwise = insert t xs

-- Aufgabe 01B: Klassische Notation
treeEqual :: Tree -> Tree -> Bool
treeEqual NIL NIL = True
treeEqual NIL (Node x t1 t2) = False
treeEqual (Node x t1 t2) NIL = False
treeEqual (Node x t1 t2) (Node y ta tb) = (x == y) && (treeEqual t1 ta) && (treeEqual t2 tb)

-- Aufgabe 01B: Operator-/Infixnotation
treeEq :: Tree -> Tree -> Bool
treeEq NIL NIL = True
treeEq NIL (Node x t1 t2) = False
treeEq (Node x t1 t2) NIL = False
treeEq (Node x t1 t2) (Node y ta tb) = (x == y) && (t1 `treeEq` ta) && (t2 `treeEq` tb)
