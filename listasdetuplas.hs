--

-- 1)
somaQuadrupla :: [(Int, Int, Int, Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla ((a,b,c,d):xs) = a + b + c + d + somaQuadrupla xs

-- 2)
somaTuplas :: [((Int, Int),(Int, Int))] -> Int
somaTuplas [] = 0
somaTuplas (((a,b),(c,d)):xs) = a + b + c + d + somaTuplas xs


-- 3)
zipp :: [Int] -> [Int] -> [(Int, Int)]
zipp [] [] = []
zipp [] (x:xs) = []
zipp (x:xs) [] = []
zipp (x:xs) (y:ys) = (x, y) : zipp xs ys

-- 4)
zippTres :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
zippTres [] [] [] = []
zippTres (x:xs) [] [] = []
zippTres [] (x:xs) [] = []
zippTres [] [] (x:xs) = []
zippTres (x:xs) (y:ys) [] = []
zippTres [] (x:xs) (y:ys) = []
zippTres (y:ys) [] (x:xs) = []
zippTres (x:xs) (y:ys) (z:zs) = (x, y, z) : zippTres xs ys zs

-- 5)
unZipp :: [(Int, Int)] -> ([Int],[Int])
unZipp [] = ([],[])
unZipp (x:xs) = (unzipEsq (x:xs), unzipDir (x:xs))
    where
        unzipEsq :: [(Int, Int)] -> [Int]
        unzipEsq [] = []
        unzipEsq ((a,b):xs) = a : unzipEsq xs

        unzipDir :: [(Int, Int)] -> [Int]
        unzipDir [] = []
        unzipDir ((a,b):xs) = b : unzipDir xs
