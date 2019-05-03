-- Übung 04 - Programmierung TU Dresden
-- Tutor: Eric Kunze
-- Github: https://github.com/oakoneric/programmierung-ss19


-- Aufgabe 02A
unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ls = (left ls, right ls)
    where
        left [] = []
        left ((a, _):ls) = a : left ls
        right [] = []
        right ((_, a):ls) = a : right ls

-- Aufgabe 02A: Einzelne Hilfsfunktion
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ls = f [] [] ls
    where
        f x y [] = (x, y)
        f x y ((a, b):ls) = f (x ++ [a]) (y ++ [b]) ls

unzip'' :: [(a, b)] -> ([a], [b])
unzip'' [] = ([], [])
unzip'' ((x, y):ls) = (x:xs, y:ys)
    where
        (xs, ys) = unzip'' ls

-- Aufgabe 02B
{-
map (uncurry (+)) [(1, 2), (3, 4)]
= uncurry (+) (1, 2) : map (uncurry (+)) [(3, 4)]
= (+) 1 2 : uncurry (+) (3, 4) : map (uncurry (+)) []
= 3 : (+) 3 4 : []
= 3 : 7 : []
= [3, 7]
-}
