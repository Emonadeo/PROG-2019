-- Übung 03 - Programmierung TU Dresden
-- Tutor: Eric Kunze
-- Github: https://github.com/oakoneric/programmierung-ss19

import Prelude hiding (even)

data Tree = Node Int [Tree]

-- Aufgabe 02A
nLeaves :: Tree -> Int
nLeaves (Node _ []) = 1
nLeaves (Node _ xxs) = f xxs
    where
        f [] = 0
        f (x:xs) = (nLeaves x) + f xs

-- Aufgabe 02A: Einzelne Funktion
nLeaves' :: Tree -> Int
nLeaves' (Node _ []) = 1
nLeaves' (Node _ [x]) = nLeaves' x
nLeaves' (Node a (x:xs)) = nLeaves' (Node a [x]) + nLeaves' (Node a xs)

-- Aufgabe 02A: Funktion höherer Ordnung
nLeaves'' :: Tree -> Int
nLeaves'' (Node _ []) = 1
nLeaves'' (Node _ ts) = foldr (+) 0 (map nLeaves'' ts)

-- Aufgabe 02B
even :: Tree -> Bool
even (Node a ts) = (length ts) `mod` 2 == 0 && evens ts
    where
        evens [] = True
        evens (x:xs) = even x && evens xs

-- Aufgabe 02B: Einzelne Funktion
even' :: Tree -> Bool
even' (Node _ []) = True
even' (Node _ [x]) = False
even' (Node a (x:y:xs)) = even' x && even' y && even' (Node a xs)

-- Aufgabe 02B: Funktion höherer Ordnung
even'' :: Tree -> Bool
even'' (Node a ts) = (length ts) `mod` 2 == 0 && foldr (&&) True (map even'' ts)