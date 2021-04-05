--

--Função mini e maxi requisitadas no exericicio 3
maxi :: Int -> Int -> Int
maxi x y
    |   (x > y) = x
    |   (x < y) = y
    |   otherwise = x

mini :: Int -> Int -> Int
mini x y
    |   (x < y) = x
    |   (x > y) = y
    |   otherwise = x

-- Função vendas
vendas :: Int -> Int
vendas 0 = 6
vendas 1 = 4
vendas 2 = 9
vendas 3 = 7
vendas 4 = 3
vendas 5 = 0
vendas 6 = 4
vendas _ = 10

-- 1)
somaTuplas :: ((Int, Int), (Int, Int)) -> Int
somaTuplas ((a, b), (c, d)) = a+b+c+d

-- 2)
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) = (a, (b, c));

-- 3)
minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = ((min (min b c) a), (maxi (maxi b c) a))

-- 4)
zeroVenda :: Int -> (Int, Bool)
zeroVenda n
    |   (vendas n) == 0 = (n, True)
    |   n > 0 = zeroVenda (n-1)
    |   otherwise = (-1, False)

-- 5) 
type Livro = (String, String, Int)

livro1 :: Livro
livro1 = ("Revolucao dos Bichos", "George Orwell", 9788535909555)

retornaTitulo :: Livro -> String
retornaTitulo (a,b,c) = a

retornaAutor :: Livro -> String
retornaAutor (a,b,c) = b 

retornaIsbn :: Livro -> Int
retornaIsbn (a,b,c) = c
