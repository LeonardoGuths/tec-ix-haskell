-- Leonardo dos Santos Guths | Matrícula: 19100392

-- 1)
--head :: [t] -> t

--tail :: [t] -> t

--fst :: (a, b) -> a

--shift :: ((a,b),c) -> (a,(b,c))

-- 2)
--concatena :: [t] -> [t]

-- 3)
--inverte :: [t] -> [t]

-- 4)
--zipp3 :: [a] -> [b] -> [c] -> [(a,b,c)]

-- 5)
--mapMaisUm :: (a -> b) -> [a] -> [b]

-- 6)
folder :: (a -> b -> b) -> b -> [a] -> b
folder f v [] = v
folder f v (x:xs) = f x (folder f v xs)

funcao :: a -> b -> b
funcao x y = y
