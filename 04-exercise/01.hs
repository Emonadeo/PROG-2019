-- Übung 04 - Programmierung TU Dresden
-- Tutor: Eric Kunze
-- Github: https://github.com/oakoneric/programmierung-ss19


data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving (Show)

-- Aufgabe 01A
rhabarber :: Tree Int
rhabarber = Branch 1
        (Branch 2
            (Leaf 3)
            (Branch 4
                (Leaf 5)
                (Leaf 6)
            )
        )
        (Branch 7
            (Branch 8
                (Leaf 9)
                (Leaf 10)
            )
            (Leaf 11)
        )

-- Aufgabe 01B
depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch _ l r) = 1 + min (depth l) (depth r)

-- Aufgabe 01C
paths :: Tree a -> Tree [a]
paths t = paths' [] t
    where
        paths' ls (Leaf a) = Leaf (ls ++ [a])
        paths' ls (Branch a l r) =
            let ls' = ls ++ [a]
            in Branch ls' (paths' ls' l) (paths' ls' r)

-- Aufgabe 01D
tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf a) = Leaf (f a)
tmap f (Branch a l r) = Branch (f a) (tmap f l) (tmap f r)