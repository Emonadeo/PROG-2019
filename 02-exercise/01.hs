-- Übung XX - Programmierung TU Dresden
-- Tutor: Claas de Boer
-- Github: https://github.com/cdboer/programmierung-ss19

-- Aufgabe 01A
pack :: [Char] -> [[Char]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = unite x (x:xs) : find x xs

unite :: Char -> [Char] -> [Char]
unite _ [] = []
unite y (x:xs)
    | y == x = x : unite y xs
    | otherwise = []

find :: Char -> [Char] -> [[Char]]
find _ [] = []
find y (x:xs)
    | y == x = find y xs
    | otherwise = pack (x:xs)

-- Aufgabe 01B
encode :: [Char] -> [(Int, Char)]
encode xs = measure (pack xs)
    where
        measure [] = []
        measure ((x:xs):ys) = (length (x:xs), x) : measure ys

-- Aufgabe 01C
decode :: [(Int, Char)] -> [Char]
decode [] = []
decode ((n, x):xs) = expand n x ++ decode xs
    where
        expand 0 _ = []
        expand n x = x : expand (n-1) x


-- Aufgabe 01D
rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xxs @ (x:xs) n
    | n == 0 = xxs
    | n > 0 = rotate (xs ++ [x]) (n-1)
    | otherwise = rotate (last xxs: take (length xxs - 1) xxs) (n+1)

rotate' :: [Int] -> Int -> [Int]
rotate' [] _ = []
rotate' xxs @ (x:xs) n
    | n == 0 = xxs
    | n > 0 = rotate' (xs ++ [x]) (n-1)
    | otherwise = rotate' xxs (length xxs + n)