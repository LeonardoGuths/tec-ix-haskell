-- Guths

-- Jogo da Memória - Trabalho Final de TEC IX com o André Du Bois
import System.IO
import System.Console.ANSI
--import System.Random

--newRand = randomIO :: IO Int
--IntRand = randomR (0, 10) :: newStdGen
{-
pegaRandom :: IO ()
pegaRandom = do
             g <- newStdGen
             randomR (1, 10) g
-}

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

cartaMais :: Carta
cartaMais = [['[','+','+',']'],
             ['[','+','+',']']]

cartaInte :: Carta
cartaInte = [['[','?','?',']'],
             ['[','?','?',']']]

cartaArrob :: Carta
cartaArrob = [['[','@','@',']'],
              ['[','@','@',']']]

cartaO :: Carta
cartaO = [['[','O','O',']'],
          ['[','O','O',']']]

cartaPorc :: Carta
cartaPorc = [['[','%','%',']'],
             ['[','%','%',']']]

cartaG :: Carta
cartaG = [['[','G','G',']'],
          ['[','G','G',']']]

tBase8 :: [Carta]
tBase8 = [cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC]

tBase16 :: [Carta]
tBase16 = [cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC, cartaC]

tabuleiroV8 :: [Carta]
tabuleiroV8 = [cartaV, cartaV, cartaV, cartaV, cartaV, cartaV, cartaV, cartaV]

tabuleiro18 :: [Carta]
tabuleiro18 = [cartaX, cartaX, cartaCifrao, cartaInte, cartaMais, cartaCifrao, cartaMais, cartaInte]

tabuleiro116 :: [Carta]
tabuleiro116 = [cartaX, cartaG, cartaCifrao, cartaInte, cartaMais, cartaO, cartaArrob, cartaPorc, cartaX, cartaO, cartaCifrao, cartaArrob, cartaG, cartaMais, cartaPorc, cartaInte]


-----------------------------------------------------------
{-
MODO DE PRINTAR ANTIGO - SE TORNOU OBSOLETO
printCarta :: Int -> Carta -> String
printCarta a [] = []
printCarta a (x:xs)
    |   xs /= [] = printaLinha x ++ " " ++ show a ++ "\n" ++ printCarta 0 xs
    |   otherwise = printaLinha x ++ "\n----"

printTabuleiro :: Int -> [Carta] -> String
printTabuleiro a [] = []
printTabuleiro a (x:xs) = printCarta a x ++ "\n" ++ printTabuleiro (a+1) xs
-}

printaLinha :: [Char] -> String
printaLinha [] = []
printaLinha (x:xs) = x : printaLinha xs

printCarta2 :: Int -> Carta -> IO ()
printCarta2 a [] = do
                    putStr " "
printCarta2 a (x:xs) = do
                    if (xs /= [])
                    then do
                        if (a == 1)
                        then do
                            putStr (printaLinha x ++ " " ++ show a ++ "\n")
                            printCarta2 0 xs
                        else do
                            if (a <= 9)
                            then do
                                putStr (printaLinha x ++ " " ++ show a)
                                hCursorDown stdout 1
                                hCursorBackward stdout 6
                                printCarta2 0 xs
                            else do
                                putStr (printaLinha x ++ " " ++ show a)
                                hCursorDown stdout 1
                                hCursorBackward stdout 7
                                printCarta2 0 xs
                    else do
                        putStr (printaLinha x)

printT2 :: Int -> [Carta] -> IO ()
printT2 n [] = do
                putStr "\n\n"
printT2 n (x:xs) = do
                 printCarta2 n x
                 hCursorUp stdout 1
                 hCursorForward stdout 6
                 if ((n == 4 || n == 8 || n == 12) && xs /= [])
                 then do
                     putStr "\n\n\n\n"
                     hCursorUp stdout 1
                 else do
                     putStr ""
                 printT2 (n+1) xs

------------------------------

restaCartas :: [Carta] -> Bool
restaCartas [] = False
restaCartas (x:xs)
    |   x /= cartaV = True
    |   otherwise = restaCartas xs

posicaoValida :: Int -> [Carta] -> Bool
posicaoValida x t
    |   x <= (tamanhoTabuleiro t) = True
    |   otherwise = False

tamanhoTabuleiro :: [Carta] -> Int
tamanhoTabuleiro [] = 0
tamanhoTabuleiro (x:xs) = 1 + tamanhoTabuleiro xs

gArr :: Int -> [t] -> t
gArr 0 (x:xs) = x
gArr n (x:xs) = gArr (n-1) xs

uArr :: Int -> a -> [a] -> [a]
uArr 0 z (x:xs) = z : xs
uArr n z (x:xs) = x : uArr (n-1) z xs

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
    clearScreen
    setSGR [SetColor Foreground Vivid Yellow]
    putStr "Seja bem-vindo ao jogo da memória!\n"
    putStr "Para começar, escolha o tamanho de tabuleiro que você deseja!\n"
    putStr "As opções são: 8 e 16!\n"
    putStr "Sua escolha: "
    escolha <- getLine
    let e = read escolha
    setSGR [Reset]
    if (e == 8)
    then do
        gameLoop tabuleiro18 tBase8
    else do
        if (e == 16)
        then do
            gameLoop tabuleiro116 tBase16
        else do
            repeteescolha
        

repeteescolha :: IO ()
repeteescolha = do
    setSGR [SetColor Foreground Vivid Red]
    putStr "Você digitou um valor inválido!\n"
    setSGR [SetColor Foreground Vivid Yellow]
    putStr "Escolha (8 ou 16): "
    escolha <- getLine
    let e = read escolha
    setSGR [Reset]
    if (e == 8)
    then do
        gameLoop tabuleiro18 tBase8
    else do
        if (e == 16)
        then do
            gameLoop tabuleiro116 tBase16
        else do
            repeteescolha

gameLoop :: [Carta] -> [Carta] -> IO ()
gameLoop tG tB = do
  putStr "\n" 
  if (not (restaCartas tG))
  then do
        --putStr (printBoard gb)
        setSGR [SetColor Foreground Vivid Green]
        putStr "!!!!!!!!!!!!!!!! - VENCEU - !!!!!!!!!!!!!!!!!\n"
        setSGR [SetColor Foreground Vivid Cyan]
        putStr "!!!!!!!!!!!!!!! - PARABENS - !!!!!!!!!!!!!!!!\n"
        setSGR [SetColor Foreground Vivid Magenta]
        putStr "!!!!!!!!!!!!!! - TUDO DE BOM - !!!!!!!!!!!!!!\n"
        setSGR [Reset]
        putStr "Deseja iniciar um novo jogo? (1 = Sim, qualquer outro número = Não)\n"
        escolha <- getLine
        let e = read escolha
        if (e == 1)
        then do
            main
        else do
            putStr "\nAté a próxima!\n"
    else do
        --clearScreen
        setSGR [SetColor Foreground Vivid Yellow]
        putStr "--------------------------------------\n"
        printT2 1 tB
        putStr "--------------------------------------\n"
        setSGR [Reset]
        putStr "Qual carta voce quer virar?\n"
        putStr "Digite uma carta: "
        carta1 <- getLine
        let c1 = read carta1
        --putStr ("carta escolhida > " ++ soPrinta c1 ++ "\n")
        if (posicaoValida c1 tG)
        then do
            --putStr ("IF > " ++ soPrinta c1 ++ "\n")
            if (temCarta c1 tG)
            then do
                --putStr ("TEM CARTA! " ++ soPrinta c1 ++ "\n")
                clearScreen
                setSGR [SetColor Foreground Vivid Yellow]
                putStr "--------------------------------------\n"
                printT2 1 (revela1Carta c1 tG tB)
                putStr "--------------------------------------\n"
                setSGR [Reset]
                putStr "Qual outra carta voce quer virar?\n"
                putStr "Digite uma carta: "
                carta2 <- getLine
                let c2 = read carta2
                --putStr ("NOVA carta escolhida > " ++ soPrinta c2 ++ "\n")
                if (posicaoValida c2 tG)
                then do
                    if (temCarta c2 tG)
                    then do
                        if (c1 == c2)
                        then do
                            --clearScreen
                            setSGR [SetColor Foreground Vivid Red]
                            putStr "\nVocê não pode escolher duas vezes a mesma carta!\n\n"
                            setSGR [Reset]
                            putStr "continue... >\n(pressione qualquer tecla) >"
                            x <- getLine
                            clearScreen
                            gameLoop tG tB
                        else do
                            if (comparaCartas (retornaCarta c1 tG) (retornaCarta c2 tG))
                            then do
                                clearScreen
                                setSGR [SetColor Foreground Vivid Green]
                                putStr "--------------------------------------\n"
                                printT2 1 (revela2Carta c1 c2 tG tB)
                                putStr "--------------------------------------\n"
                                putStr ("* * * AS CARTAS SÃO IGUAIS * * *\n")
                                setSGR [Reset]
                                putStr "continue... >\n(pressione qualquer tecla) >"
                                x <- getLine
                                clearScreen
                                --hCursorUp stdout 48
                                gameLoop (removeCartas c1 c2 tG) (removeCartas c1 c2 tB)
                            else do
                                clearScreen
                                setSGR [SetColor Foreground Vivid Red]
                                putStr "--------------------------------------\n"
                                printT2 1 (revela2Carta c1 c2 tG tB)
                                putStr "--------------------------------------\n"
                                putStr ("- - - AS CARTAS NAO SÃO IGUAIS - - -\n")
                                setSGR [Reset]
                                putStr "continue... >\n(pressione qualquer tecla) >"
                                x <- getLine
                                clearScreen
                                --hCursorUp stdout 48
                                gameLoop tG tB
                    else do
                        clearScreen
                        setSGR [SetColor Foreground Vivid Red]
                        putStr ("\nA posição " ++ soPrinta c2 ++ " já está vazia!\n")
                        setSGR [Reset]
                        gameLoop tG tB
                else do
                    clearScreen
                    setSGR [SetColor Foreground Vivid Red]
                    putStr ("\nA posição " ++ soPrinta c2 ++ " não é válida!\n")
                    setSGR [Reset]
                    gameLoop tG tB
            else do
                clearScreen
                setSGR [SetColor Foreground Vivid Red]
                putStr ("\nA posição " ++ soPrinta c1 ++ " já está vazia!\n")
                setSGR [Reset]
                gameLoop tG tB
         else do
            clearScreen
            setSGR [SetColor Foreground Vivid Red]
            putStr ("\nA posição " ++ soPrinta c1 ++ " não é válida!\n")
            setSGR [Reset]
            gameLoop tG tB

             