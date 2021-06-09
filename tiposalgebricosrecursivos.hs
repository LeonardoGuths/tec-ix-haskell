-- 

data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
    deriving (Eq,Show)

-----------------------------ÁRVORES PARA REALIZAÇÃO DE TESTES--------------------------------
--arv1 :: Arvore Int
--arv1 = Nodo 2 (Nodo 4 (Folha 6) (Folha 8)) (Nodo 1 (Folha 3) (Folha 7))

--arv2 :: Arvore Int
--arv2 = (Nodo 5 (Nodo 4 (Nodo 3 (Nodo 2 (Folha 1) (Folha 1)) (Folha 1)) (Folha 4)) (Folha 5))
----------------------------------------------------------------------------------------------

-- 1)
multDois :: Arvore Int -> Arvore Int
multDois (Folha n) = (Folha (n*2))
multDois (Nodo n a1 a2) = (Nodo (n*2) (multDois a1) (multDois a2))

-- 2)
contaElementos :: Arvore a -> Int
contaElementos (Folha n) = 1
contaElementos (Nodo n a1 a2) = 1 + contaElementos a1 + contaElementos a2

-- 3)
altura :: Arvore a -> Int
altura (Folha n) = 1
altura (Nodo n a1 a2) 
    |   altura a1 > altura a2 = 1 + altura a1
    |   otherwise = 1 + altura a2

-- 4)
maiorElemento :: Arvore Int -> Int
maiorElemento (Folha n) = n
maiorElemento (Nodo n a1 a2)
    |   (n > maiorElemento a1) && (n > maiorElemento a2) = n
    |   maiorElemento a1 > maiorElemento a2 = maiorElemento a1
    |   otherwise = maiorElemento a2

-- 5)
procuraInt :: Int -> Arvore Int -> Bool
procuraInt x (Folha n)
    |   x == n = True
    |   otherwise = False

procuraInt x (Nodo n a1 a2)
    |   x == n = True
    |   procuraInt x a1 = True
    |   procuraInt x a2 = True
    |   otherwise = False

-- 6)
quantasVezes :: Int -> Arvore Int -> Int
quantasVezes x (Folha n)
    |   x == n = 1
    |   otherwise = 0

quantasVezes x (Nodo n a1 a2)
    |   x == n = 1 + quantasVezes x a1 + quantasVezes x a2
    |   otherwise = quantasVezes x a1 + quantasVezes x a2

-- 7)
refleteArvore :: Arvore a -> Arvore a
refleteArvore (Folha n) = (Folha n)
refleteArvore (Nodo n a1 a2) = (Nodo n (refleteArvore a2) (refleteArvore a1))

-- 8)
arvoreToLista :: Arvore a -> [a]
arvoreToLista (Folha n) = n : []
arvoreToLista (Nodo n a1 a2) = [n] ++ (arvoreToLista a1) ++ (arvoreToLista a2)

-- 9)
mapTree :: (a -> b) -> Arvore a -> Arvore b
mapTree f (Folha n) = (Folha (f n))
mapTree f (Nodo n a1 a2) = (Nodo (f n) (mapTree f a1) (mapTree f a2))
