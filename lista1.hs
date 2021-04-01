

--Função tresIguais dos slides da aula, solicitado no exercício 5.
tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (y == z)

-- 1)
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais w x y z = (w == x) && (x == y) && (y == z);

-- 2)
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z
    |   (x == y) && (y == z) = 3
    |   (x == y) || (y == z) || (x == z)= 2
    |   otherwise = 0

-- 3)
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = (x /= y) && (y /= z) && (x /= z)

-- 4) A seguinte definição retornará True (verdadeiro) caso n e p forem iguais, que pela lógica é um caso falso.

-- 5)
quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 x y z
    |   todosDiferentes x y z = 0
    |   tresIguais x y z = 3
    |   otherwise = 2

-- 6)
elevadoDois :: Int -> Int
elevadoDois x = x * x

-- 7)
elevadoQuatro :: Int -> Int
elevadoQuatro x = elevadoDois x * elevadoDois x

-- 8)
vendas :: Int -> Int
vendas 0 = 2
vendas 1 = 4
vendas 2 = 6
vendas 3 = 8
vendas _ = 10

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal (n-1)