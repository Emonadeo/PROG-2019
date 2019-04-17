-- Übung 01 - Programmierung TU Dresden
-- Tutor: Claas de Boer
-- Github: https://github.com/cdboer/programmierung-ss19 

-- Aufabe ad01:
-- Implementieren Sie die (unendliche) Liste der Fibonacci-Zahlen f0, f1, ...

fibs :: [Int]
fibs = fibs' 0
    where
        fibs' x = fib x: fibs' (x+1)

fib :: Int -> Int
fib n = fib' 1 1 n

fib' :: Int -> Int -> Int -> Int
fib' x _ 0 = x
fib' x y n = fib' y (x + y) (n - 1)

fibs'' :: [Int]
fibs'' = [fib i | i <- [0..]]

-- Aufgabe ad02:
-- In einem vollen Binärbaum ist jeder Knoten entweder ein Blatt, oder er hat zwei Kinderknoten.
-- Implementieren Sie eine Funktion, welche für n ∈ ℕ die Anzahl der vollen Binärbäume mit Knotenzahl n berechnet.

-- TODO
