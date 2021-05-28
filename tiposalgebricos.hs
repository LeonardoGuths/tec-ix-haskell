-- 

-- 1)
data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado
    deriving (Eq, Show)

-- 2)
finalDeSemana :: Dia -> Bool
finalDeSemana Sabado = True
finalDeSemana Domingo = True
finalDeSemana _ = False

-- 3)
data TalvezFloat = Valor Float | Erro String
    deriving (Eq, Show)

-- 4)
divisao :: Float -> Float -> TalvezFloat
divisao x 0 = Erro "mensagem de erro apropriada"
divisao x y = Valor (x/y)

-- 5)
data Nat = Zero | Suc Nat
    deriving (Eq, Show)

dois :: Nat
dois = Suc (Suc Zero)

sete :: Nat
sete = Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero))))))

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc x) = 1 + natToInt x

-- 6)
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = Suc (intToNat(x-1))
