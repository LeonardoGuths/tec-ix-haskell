--

-- 1)
membro :: Int -> [Int] -> Bool
membro a [] = False
membro a (x:xs)
    |   a == x = True
    |   otherwise = membro a xs

-- 2)
membroNum :: Int -> [Int] -> Int
membroNum a [] = 0
membroNum a (x:xs)
    |   a == x = 1 + membroNum a xs
    |   otherwise = 0 + membroNum a xs

-- 3)
membro2 :: Int -> [Int] -> Bool
membro2 a (x:xs)
    |   (membroNum a (x:xs) == 0) = False
    |   otherwise = True

-- 4)
unico :: [Int] -> [Int]
unico [] = []
unico (x:xs) = unico2 (x:xs) (x:xs)

unico2 :: [Int] -> [Int] -> [Int]
unico2 [] (y:ys) = []
unico2 (x:xs) (y:ys)
    |   (membroNum x (y:ys) == 1) && (membroNum x xs == 0) = x : unico2 xs (y:ys)
    |   otherwise = unico2 xs (y:ys)

-- 5)
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores a [] = []
menores a (x:xs)
    |   a >= x = x : menores a xs
    |   otherwise = menores a xs

maiores :: Int -> [Int] -> [Int]
maiores a [] = []
maiores a (x:xs)
    |   a <= x = x : maiores a xs
    |   otherwise = maiores a xs
