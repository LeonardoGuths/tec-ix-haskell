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
maxVenda n
    |   (vendas n) == (maiorVenda n) = n
    |   otherwise = maxVenda (n-1)

-- 4)
zeroVendas :: Int -> Int
zeroVendas n
    |   ((vendas n) == 0) = n
    |   n > 0 = zeroVendas (n-1)
    |   otherwise = -1

-- 5)
achaSemana :: Int -> Int -> Int
achaSemana s n
    |   (vendas n) == s = n
    |   n > 0 = achaSemana s (n-1)
    |   otherwise = -1

-- 6)
zeroVendas2 :: Int -> Int
zeroVendas2 n
    |   (vendas n) == 0 = n
    |   n > 0 = achaSemana 0 (n-1)
    |   otherwise = -1

-- 7)
maiorVenda2 :: Int -> Int -> Int
maiorVenda2 m n
    |   n > m = maxi (vendas n) (maiorVenda2 m (n-1))
    |   otherwise = vendas m

maxVenda2 :: Int -> Int -> Int
maxVenda2 m n
    |   (n >= m) && ((vendas n) == (maiorVenda2 m n)) = n
    |   otherwise = maxVenda2 m (n-1)

zeroVendas3 :: Int -> Int -> Int
zeroVendas3 m n
    |   ((vendas n) == 0) = n
    |   n > m = zeroVendas3 m (n-1)
    |   otherwise = -1

achaSemana2 :: Int -> Int -> Int -> Int
achaSemana2 s m n
    |   (vendas n) == s = n
    |   n > m = achaSemana2 s m (n-1)
    |   otherwise = -1

zeroVendas4 :: Int -> Int -> Int
zeroVendas4 m n
    |   (vendas n) == 0 = n
    |   n > m = achaSemana2 0 m (n-1)
    |   otherwise = -1

-- 8)
fatorial :: Int -> Int
fatorial 1 = 1
fatorial x = x * fatorial (x-1)

-- 9)
produto :: Int -> Int -> Int
produto m n
    |   n > m = n * produto m (n-1)
    |   otherwise = n

-- 10)
fib :: Int -> Int
fib 0 = 
fib n
    |   