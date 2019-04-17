-- Übung 02 - Programmierung TU Dresden
-- Tutor: Claas de Boer
-- Github: https://github.com/cdboer/programmierung-ss19

-- Aufgabe 02:
-- Implementieren Sie eine Funktion die bei Eingabe der Zahl i die i-te Zahl der Fibonacci Folge berechnet.
-- z.B.: fib 3 = 2 oder fib 5 = 8
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)
