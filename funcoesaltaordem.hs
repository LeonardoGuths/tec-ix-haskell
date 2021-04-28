--

-- Função vendas da lista 1, solicitado no exercício 2.
vendas :: Int -> Int
vendas 0 = 6
vendas 1 = 4
vendas 2 = 9
vendas 3 = 7
vendas 4 = 8
vendas 5 = 0
vendas 6 = 4
vendas _ = 10

-- 1)
aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes f x = f (f x)

-- 2)
vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal f 0 = f 0
vendaTotal f x = f x + (vendaTotal f (x-1))

-- 3)
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "Lista vazia!"
foldInt f (x:[]) = x
foldInt f (x:xs) = f x (foldInt f xs)

soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x y = x * y

-- 4)
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs)
    |   f x = x : filterString f xs
    |   otherwise = filterString f xs

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

-- 5)
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs

quadrado :: Int -> Int
quadrado x = x * x

somaQuadrado :: [Int] -> Int
somaQuadrado [] = error "Lista vazia!"
somaQuadrado (x:xs) = foldInt soma (mapInt quadrado (x:xs))

-- 6)
iter :: Int -> (Int -> Int) -> Int -> Int
iter 0 f x = x
iter a f x = f (iter (a-1) f x)
