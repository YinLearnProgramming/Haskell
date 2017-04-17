letter :: Int -> Char
letter n	| n >= 90 = 'A'
		| n >= 80 = 'B'
		| n >= 70 = 'C'
		| n >= 60 = 'D'
		| otherwise = 'F'

grade :: [Int] -> [Char]

grade xs = map letter xs

sort :: Ord a => [a] -> [a]
sort []     = []
sort (p:xs) = (sort lesser) ++ [p] ++ (sort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

freq x xs = length [ True | y <- xs, x ==y]

histogram [] = []
histogram (x:xs) = (x, freq x (x: sort xs)) :
		 histogram (filter (/=x) (x:sort xs))

allOcrDelete :: Eq a => a -> [a] -> [a]
allOcrDelete _ [] = []
allOcrDelete del (x:xs)
    | del == x  =   allOcrDelete del xs
    | otherwise = x:allOcrDelete del xs

coPrime :: Int -> Int -> Bool
coPrime x y = gcd x y == 1
