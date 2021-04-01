
-- Função vendas da lista 1, solicitado no exercício 2.
vendas :: Int -> Int
vendas 0 = 2
vendas 1 = 4
vendas 2 = 9
vendas 3 = 8
vendas _ = 10

-- 1)
maxi :: Int -> Int -> Int
maxi x y
    |   (x > y) = x
    |   (x < y) = y
    |   otherwise = x

-- 2)
maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maxi (vendas n) (maiorVenda (n-1))

-- 3)
maxVenda :: Int -> Int
