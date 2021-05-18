--

-- Jogo Resta Um

module RestaUm where

import System.IO

type GBoard = [[Char]]

gBoard :: GBoard
gBoard = [[' ',' ','*','*','*',' ',' '],
          [' ',' ','*','*','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          ['*','*','*',' ','*','*','*'],
          ['*','*','*','*','*','*','*'],
          [' ',' ','*','*','*',' ',' '],
          [' ',' ','*','*','*',' ',' ']]

tBoard :: GBoard
tBoard = [[' ',' ','*',' ','*',' ',' '],
          [' ',' ','*',' ','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          ['*',' ','*',' ','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          [' ',' ','*',' ','*',' ',' '],
          [' ',' ','*','*','*',' ',' ']]

umBoard :: GBoard
umBoard = [[' ',' ',' ',' ',' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' '],
          [' ',' ',' ','*',' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ']]

eBoard :: GBoard
eBoard = ["  ***  ",
          "  * *  ",
          "*** ***",
          "*******",
          "*******",
          "  ***  ",
          "  ***  "] 
---------- Tabuleiros de teste -----------------

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

isValidPos :: (Int,Int) -> Bool
isValidPos (l,c) 
    |   (l >= 2 && l <= 4) && (c >= 0 && c <= 6) = True
    |   (l >= 0 && l <= 6) && (c >= 2 && c <= 4) = True
    |   otherwise = False

moves :: Int -> Int -> [[(Int,Int)]]
moves l c = [(l, c-1),(l, c-2)] : [(l, c+1),(l, c+2)] : [(l+1, c),(l+2, c)] : [(l-1, c),(l-2, c)] : []

isValidPosMove :: [(Int,Int)] -> Bool
isValidPosMove [] = True
isValidPosMove (x:xs) = (isValidPos x) && (isValidPosMove xs)

isEmpty :: (Int,Int) -> GBoard -> Bool
isEmpty (l,c) (x:xs)
     |    (gPos (l,c) (x:xs)) == ' ' = True
     |    otherwise = False

isValidMove :: GBoard -> [(Int,Int)] -> Bool
isValidMove (x:xs) (m:ms)
     |    (gPos m (x:xs) == '*') && (isValidMove2 (x:xs) ms) = True
     |    otherwise = False
     where
          isValidMove2 :: GBoard -> [(Int,Int)] -> Bool
          isValidMove2 (x:xs) (m:ms)
               |    (gPos m (x:xs) == ' ') = True
               |    otherwise = False

validMoves :: GBoard -> (Int, Int) -> [[(Int,Int)]]
validMoves (x:xs) (l,c)
     |    isValidPos (l,c) = (filter (isValidMove (x:xs)) (filter isValidPosMove (moves l c)))
     |    otherwise = []

move :: (Int,Int) -> [(Int,Int)] -> GBoard -> GBoard
move (l,c) (m:ms) (x:xs) = uPos (l,c) ' ' (uPos m ' ' (updateMove2 ms (x:xs)))
     where
          updateMove2 :: [(Int, Int)] -> GBoard -> GBoard
          updateMove2 (m:ms) (x:xs) = uPos m '*' (x:xs) 

genTabPositions :: GBoard -> [[(Int,Int)]]
genTabPositions (x:xs) = genTabPositions2 (x:xs) 0 
     where
          genTabPositions2 :: GBoard -> Int -> [[(Int,Int)]]
          genTabPositions2 [] l = []
          genTabPositions2 (x:xs) l = genLinha x l 0 : genTabPositions2 xs (l+1)
               where
                    genLinha :: [Char] -> Int -> Int -> [(Int,Int)]
                    genLinha [] l c = []
                    genLinha (x:xs) l c
                         |    (x == ' ') || (x == '*') = (l, c) : genLinha xs l (c+1)
                         |    otherwise = []

canMove :: GBoard -> Bool
canMove (x:xs) = verLinha (x:xs) (genTabPositions (x:xs))
     where
          verLinha :: GBoard -> [[(Int,Int)]] -> Bool
          verLinha (x:xs) [] = False
          verLinha (x:xs) (p:ps) = (verifica (x:xs) p) || (verLinha (x:xs) ps)
               where
                    verifica :: GBoard -> [(Int,Int)] -> Bool
                    verifica (x:xs) [] = False
                    verifica (x:xs) (p:ps)
                         |    not(isEmpty p (x:xs)) && ((validMoves (x:xs) p) /= []) = True
                         |    otherwise = verifica (x:xs) ps

restaUm :: GBoard -> Bool
restaUm (x:xs)
     |    foldr (+) 0 (map somaSterisco (x:xs)) == 1 = True
     |    otherwise = False
     where
          somaSterisco :: [Char] -> Int
          somaSterisco [] = 0
          somaSterisco (x:xs)
               |    x == '*' = 1 + somaSterisco xs
               |    otherwise = somaSterisco xs

printBoard :: GBoard -> String
printBoard (x:xs) = cabecalho ((length x)-1) ++ corpo ((length (x:xs))-1) (reverse(x:xs))
     where
          cabecalho :: Int -> String
          cabecalho 0 = "\n" ++ "  0"
          cabecalho n = cabecalho (n-1) ++ " " ++ show n 

          corpo :: Int -> GBoard -> String
          corpo 0 (x:xs) = "\n" ++ "0 " ++ (corpoEspaco x) ++ "\n"
          corpo n (x:xs) = corpo (n-1) xs ++ show n ++ " " ++ (corpoEspaco x) ++ "\n"

          corpoEspaco :: [Char] -> String
          corpoEspaco [] = ""
          corpoEspaco (x:xs) = x : " " ++ corpoEspaco xs


-- Motor do jogo
main :: IO ()
main = do
   gameLoop gBoard

gameLoop :: GBoard -> IO ()
gameLoop gb = do
   if (not (canMove gb))
   then do
        if (restaUm gb) then do
	                     putStr (printBoard gb)
	                     print "Voce Venceu!!!"
	                else do
			     putStr (printBoard gb)
			     print "Nao existem pecas para mover! Voce perdeu!!"
   else do			
      putStr (printBoard gb)
      print "Qual peca voce quer mover?"
      putStr "Digite uma linha: "
      l <- getLine
      putStr "Digite uma coluna: "
      c <- getLine
      let linha = read l
      let coluna = read c
      if isValidPos (linha,coluna)
      then case validMoves  gb (linha, coluna) of
           []  -> do 
	          print "NAO Eh POSSIVEL MOVER ESTA PECA"
	          gameLoop gb
           [l] ->do
	    --  print (show l)
	        gameLoop (move (linha,coluna) l gb)
           l   -> opcoes linha coluna l gb
      else do
           print "!!!!!POSICAO INVALIDA!!!!!"
	   gameLoop gb

opcoes linha coluna l gb = do
            print "Essa peca pode se mover para:"
	    resp <- printOpcoes 1 l
	    if (resp>=1 && resp <= length l)
	    then gameLoop (move (linha,coluna) (gArr (resp-1) l) gb)
	    else do
	         print "Opcao Invalida!!"
		 opcoes linha coluna l gb

printOpcoes n [] = do
                 putStr "Digite a opção: "
		 op <- getLine
		 let opcao = read op
		 return opcao

printOpcoes n (x:xs) = do
                     print (show n ++ ". "++ (show ((head . tail) x)))
		     printOpcoes (n+1) xs
