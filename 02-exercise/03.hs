-- Übung 02 - Programmierung TU Dresden
-- Tutor: Eric Kunze
-- Github: https://github.com/oakoneric/programmierung-ss19

data Tree = Node String [Tree]

-- Aufgabe 03A
t :: Tree
t = Node "Wurzel" [
        Node "l" [],
        Node "m" [
            Node "lu" [],
            Node "ru" []
        ],
        Node "r" []
    ]

-- Aufgabe 03B
level :: Int -> Tree -> [String]
level 0 (Node s _) = [s]
level n (Node _ []) = []
level n (Node s (t:ts)) = level (n-1) t ++ level n (Node s ts)