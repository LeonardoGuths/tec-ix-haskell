-- 

-- 1)
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao a [] = error "Lista vazia ou posição fora da lista"
pegaPosicao a (x:xs) 
    |   a > 1 = pegaPosicao (a-1) xs
    |   otherwise = x

-- 2)
pega :: Int -> [Int] -> [Int]
pega a [] = []
pega a (x:xs)
    |   a > 0 = x : pega (a-1) xs
    |   otherwise = []

-- 3)
retira :: Int -> [Int] -> [Int]
retira a [] = []
retira a (x:xs)
    |   a > 0 = retira (a-1) xs
    |   otherwise = x : retira a xs

-- 4)
mediaLista :: [Float] -> Float
mediaLista (x:xs) = (somaLista (x:xs)) / quantosLista (x:xs)
    where
        somaLista :: [Float] -> Float
        somaLista [] = 0
        somaLista (x:xs) = x + somaLista xs

        quantosLista :: [Float] -> Float
        quantosLista [] = 0
        quantosLista (x:xs) = 1 + quantosLista xs

-- 5)
pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores a [] = []
pegaMaiores a (x:xs) =  let l = (reverse (iSort (x:xs)))
                        in (pega a l)

-- Insertion Sort, utilizado no exercicio 5
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)
    where
        ins :: Int -> [Int] -> [Int]
        ins a [] = [a]
        ins a (x:xs)
            |   a <= x = a:x:xs
            |   otherwise = x: ins a xs

-- 6)
contaMaiores :: Int -> [Int] -> Int
contaMaiores a [] = 0
contaMaiores a (x:xs)
    |   x > a = 1 + contaMaiores a xs
    |   otherwise = contaMaiores a xs

-- 7)
intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala (x:xs) [] = x : intercala [] xs
intercala [] (y:ys) = y : intercala [] ys
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- 8)
dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- 9)
repli :: Int -> [Char] -> [Char]
repli a [] = []
repli a (x:xs) = repli2 a a (x:xs)
    where
        repli2 :: Int -> Int -> [Char] -> [Char]
        repli2 a b [] = []
        repli2 a b (x:xs)
            |   b > 0 = x : repli2 a (b-1) (x:xs)
            |   otherwise = repli2 a a xs

-- 10)
dropEvery :: Int -> [Char] -> [Char]
dropEvery a [] = []
dropEvery a (x:xs) = dropEvery2 a a (x:xs)
    where
        dropEvery2 :: Int -> Int -> [Char] -> [Char]
        dropEvery2 a b [] = []
        dropEvery2 a b (x:xs)
            |   b > 1 = x : dropEvery2 a (b-1) xs
            |   otherwise = dropEvery2 a a xs

-- 11)
split :: Int -> [Char] -> ([Char], [Char])
split a [] = ([],[])
split a (x:xs) = (pegaS a (x:xs), retiraS a (x:xs))
    where
        pegaS :: Int -> [Char] -> [Char]
        pegaS a [] = []
        pegaS a (x:xs)
            |   a > 0 = x : pegaS (a-1) xs
            |   otherwise = []

        retiraS :: Int -> [Char] -> [Char]
        retiraS a [] = []
        retiraS a (x:xs)
            |   a > 0 = retiraS (a-1) xs
            |   otherwise = x : retiraS a xs
