-- Guths

-- Jogo da Memória - Trabalho Final de TEC IX com o André Du Bois
import System.IO
import System.Console.ANSI
import System.Random

type Carta = [[Char]]

cartaC :: Carta
cartaC = [['[','-','-',']'],
          ['[','-','-',']']]

cartaV :: Carta
cartaV = [[' ',' ',' ',' '],
          [' ',' ',' ',' ']]

cartaX :: Carta
cartaX = [['[','X','X',']'],
          ['[','X','X',']']]

cartaCifrao :: Carta
cartaCifrao = [['[','$','$',']'],
               ['[','$','$',']']]

tBase :: [Carta]
tBase = [cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC]

tabuleiroV :: [Carta]
tabuleiroV = [cartaV, cartaV, cartaV, cartaV, cartaV, cartaV, cartaV, cartaV]

tabuleiro1 :: [Carta]
tabuleiro1 = [cartaX, cartaX, cartaCifrao, cartaX, cartaX, cartaCifrao, cartaCifrao, cartaCifrao]


-----------------------------------------------------------

printCarta :: Int -> Carta -> String
printCarta a [] = []
printCarta a (x:xs)
    |   xs /= [] = printaLinha x ++ " " ++ show a ++ "\n" ++ printCarta 0 xs
    |   otherwise = printaLinha x ++ "\n----"
    where
        printaLinha :: [Char] -> String
        printaLinha [] = []
        printaLinha (x:xs) = x : printaLinha xs

printTabuleiro :: Int -> [Carta] -> String
printTabuleiro a [] = []
printTabuleiro a (x:xs) = printCarta a x ++ "\n" ++ printTabuleiro (a+1) xs

restaCartas :: [Carta] -> Bool
restaCartas [] = False
restaCartas (x:xs)
    |   x /= cartaV = True
    |   otherwise = restaCartas xs

posicaoValida :: Int -> Bool
posicaoValida x 
    |   x <= 8 && x >=1 = True
    |   otherwise = False

gArr :: Int -> [t] -> t
gArr 0 (x:xs) = x
gArr n (x:xs) = gArr (n-1) xs

uArr :: Int -> a -> [a] -> [a]
uArr 0 z (x:xs) = z : xs
uArr n z (x:xs) = x : uArr (n-1) z xs

gPos :: (Int,Int) -> [[a]] -> a
gPos (l,c) (x:xs) = gArr c (gArr l (x:xs))

uPos :: (Int,Int) -> a -> [[a]] -> [[a]]
uPos (0,c) z (x:xs) = (uArr c z x) : xs
uPos (l,c) z (x:xs) = x : uPos (l-1,c) z xs

retornaCarta :: Int -> [Carta] -> Carta
retornaCarta n x = gArr (n-1) x

temCarta :: Int -> [Carta] -> Bool
temCarta 0 x = False
temCarta n x
    |   (gArr (n-1) x) == cartaV = False
    |   otherwise = True

soPrinta :: Int -> String
soPrinta x = show x

revela1Carta :: Int -> [Carta] -> [Carta] -> [Carta]
revela1Carta n g b = uArr (n-1) (gArr (n-1) g) b

revela2Carta :: Int -> Int -> [Carta] -> [Carta] -> [Carta]
revela2Carta n1 n2 g b = uArr (n2-1) (gArr (n2-1) g) (uArr (n1-1) (gArr (n1-1) g) b)

removeCartas :: Int -> Int -> [Carta] -> [Carta]
removeCartas n1 n2 g = uArr (n2-1) cartaV (uArr (n1-1) cartaV g)

comparaCartas :: Carta -> Carta -> Bool
comparaCartas c1 c2
    |   c1 == c2 = True
    |   otherwise = False

------------------------- Motor -----------------------

main :: IO ()
main = do
   gameLoop tabuleiro1 tBase

gameLoop :: [Carta] -> [Carta] -> IO ()
gameLoop tG tB = do
  print "-------------------------------------" 
  if (not (restaCartas tG))
  then do
        --putStr (printBoard gb)
        print "Voce Venceu!!!"
    else do
        putStr (printTabuleiro 1 tB)
        print "Qual carta voce quer virar?"
        putStr "Digite uma linha: "
        carta1 <- getLine
        let c1 = read carta1
        --putStr ("carta escolhida > " ++ soPrinta c1 ++ "\n")
        if (posicaoValida c1)
        then do
            --putStr ("IF > " ++ soPrinta c1 ++ "\n")
            if (temCarta c1 tG)
            then do
                --putStr ("TEM CARTA! " ++ soPrinta c1 ++ "\n")
                putStr (printTabuleiro 1 (revela1Carta c1 tG tB)) 
                print "Qual outra carta voce quer virar?"
                putStr "Digite uma linha: "
                carta2 <- getLine
                let c2 = read carta2
                putStr ("NOVA carta escolhida > " ++ soPrinta c2 ++ "\n")
                if posicaoValida (c2)
                then do
                    if (temCarta c2 tG)
                    then do
                        putStr (printTabuleiro 1 (revela2Carta c1 c2 tG tB))
                        if (comparaCartas (retornaCarta c1 tG) (retornaCarta c2 tG))
                        then do
                            putStr ("AS CARTA SAO IGUAL")
                            print "Pressione qualquer tecla para continuar..."
                            x <- getLine
                            gameLoop (removeCartas c1 c2 tG) (removeCartas c1 c2 tB)
                        else do
                            putStr ("AS CARTA NAO SAO IGUAL")
                            print "Pressione qualquer tecla para continuar..."
                            x <- getLine
                            gameLoop tG tB
                    else do
                        putStr ("C2 TA VAZIA > " ++ soPrinta c2 ++ "\n")
                        gameLoop tG tB
                else do
                    putStr ("C2 NAO E VALIDA > " ++ soPrinta c2 ++ "\n")
                    gameLoop tG tB
            else do
                putStr ("C1 TA VAZIA! " ++ soPrinta c1 ++ "\n")
                gameLoop tG tB
         else do
            putStr ("C1 NAO E VALIDA > " ++ soPrinta c1 ++ "\n")
            gameLoop tG tB

             