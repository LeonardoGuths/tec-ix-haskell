-- 

-- 1)
concatena :: [[a]] -> [a]
concatena (x:xs) = foldr (++) [] (x:xs)

-- 2)
andLista :: [Bool] -> Bool
andLista (x:xs) = foldr (&&) False (x:xs)

-- 3)
somaQuadPos :: [Int] -> Int
somaQuadPos (x:xs) = foldr (+) 0 (map quadrado (filter (>=0) (x:xs)))
    where
        quadrado :: Int -> Int
        quadrado x = x*x

-- 4)
somaListas :: [[Int]] -> Int
somaListas (x:xs) = foldr (+) 0 (map (foldr (+) 0) (x:xs))

-- 5)
tamanhoListas :: [[a]] -> Int
tamanhoListas (x:xs) = foldr (+) 0 (map contaqnts (x:xs))
    where
        contaqnts :: [a] -> Int
        contaqnts [] = 0
        contaqnts (x:xs) = 1 + contaqnts xs

-- 6)
inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = foldr (++) [] [inverte xs, [x]]

-- 7)
separaPalavras :: [Char] -> [[Char]]
separaPalavras [] = []
separaPalavras (x:xs) = [(takeWhile (/= ' ') (x:xs))] ++ separaPalavras (tiradafrente(dropWhile (/= ' ') (x:xs)))
    where
        tiradafrente :: [Char] -> [Char]
        tiradafrente [] = []
        tiradafrente (x:xs) 
            |   x == ' ' = xs
            |   otherwise = (x:xs)
