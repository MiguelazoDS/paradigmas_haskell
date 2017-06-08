{-# LANGUAGE Haskell98 #-}
-- cat file | ./leer línea

-- | Alumno: Cazajous Miguel A.
--
-- Asignatura: Paradigmas de programación
--
-- Docente: Ing. Wolfmann Gustavo
--
-- Tema:
--
--      - Algoritmo minimax para elección de mejor movimiento en el juego TA TE TI
--

module Main where

import Control.Monad
import Data.Char
import System.Random

-- | Sinónimo de tipo
type Tablero = String

-- | Función principal. Lee un archivo con un estado del juego,
--
-- llama a la función __'unaLinea'__ y luego a la función __'juego'__
--
main :: IO ()
main = do
    --  Lee contenido de un archivo y lo guarda en cadena
    cadena <- getContents
    let tablero = unaLinea cadena
    --  Imprime el tablero
    putStrLn "\nParadigmas de programación\n--------------------------\nTA TE TI\n--------\n"
    putStrLn "\nEstado inicial\n--------------\n"
    juego tablero


-- | Omite los espacios y saltos de línea que se leyeron del archivo
--
-- y devuelve un String con 9 valores entre __X__, __O__ y __E__
unaLinea :: Tablero -> String
unaLinea xs = [x | x <- xs, x/='\n', x/=' ']


-- | Llama a __'mostrarTablero'__ y empieza el juego con un estado recibido desde __'main'__
--
--  Verifica que si hay un ganador llamando a __'ganador'__, si lo hay, lo muestra. En caso contrario continua
--
-- Define cual es el jugador que debe continuar llamando a __'proxJugador'__
--
-- Para el caso del jugador __X__ elige una posición aleatoria entre las que son válidas (__'movPermitidos'__)
--
-- y realiza el movimiento llamando a la función __'mover'__
--
-- En el caso del jugador 'O' la posición se determina llamando a __'mejorMovimiento'__
--
-- Luego de realizado el movimiento se __'juego'__ se llama a si misma con el tablero ya modificado con el último movimiento
juego :: String -> IO()
juego tablero = do
  putStr $ mostrarTablero tablero
  if not $ formato tablero
    then error "Formato de archivo inválido"
    else do
      if (ganador tablero) /= ' '
        then putStrLn $ "\nEl ganador es el jugador " ++ show (ganador tablero) ++ "\n"
        else do
          if 'X' == (proxJugador tablero)
            then do
              putStrLn "\nTurno jugador X\n"
              num <- randomRIO (1,length (movPermitidos tablero)-1) :: IO Int
              juego (mover tablero 'X' ((movPermitidos tablero)!!num))
            else do
              putStrLn "\nTurno jugador O\n"
              juego (mover tablero 'O' (mejorMovimiento tablero))


-- | Muestra una representación del tablero considerando el estado que se leyó desde archivo
mostrarTablero :: Tablero -> String
mostrarTablero tablero =
  "+---+---+---+\n" ++
  "|   |   |   |\n" ++
  "| " ++ tablero !! 0 : [] ++ " | " ++ tablero !! 1 : [] ++ " | " ++ tablero !! 2 : []  ++ " |\n" ++
  "|   |   |   |\n" ++
  "+---+---+---+\n" ++
  "|   |   |   |\n" ++
  "| " ++ tablero !! 3 : [] ++ " | " ++ tablero !! 4 : [] ++ " | " ++ tablero !! 5 : []  ++ " |\n" ++
  "|   |   |   |\n" ++
  "+---+---+---+\n" ++
  "|   |   |   |\n" ++
  "| " ++ tablero !! 6 : [] ++ " | " ++ tablero !! 7 : [] ++ " | " ++ tablero !! 8 : []  ++ " |\n" ++
  "|   |   |   |\n" ++
  "+---+---+---+\n"


-- | Verifica que el formato del estado de juego obtenido del archivo sea válido
--
-- Esto implica que no contenga elementos diferentes a __X__, __O__ y __E__ y además que
--
-- la diferencia entre cantidad de __X__ y __O__ no sea mayor a 1
--
-- Utiliza las funciones __'cantX'__, __'cantO'__ y __'cantE'__
formato :: Tablero -> Bool
formato tablero
  | abs(x-o)<=1 && x + o + e == 9 = True
  | otherwise = False
  where
  (x,o,e)=(cantX tablero, cantO tablero, cantE tablero)


-- | Cuenta la cantidad de 'X' que tiene el estado de juego
cantX :: Tablero -> Int
cantX xs = sum [1 | x<-xs, x=='X']


-- | Cuenta la cantidad de 'O' que tiene el estado de juego
cantO :: Tablero -> Int
cantO xs = sum [1 | x<-xs, x=='O']


-- | Cuenta la cantidad de 'E' que tiene el estado de juego
cantE :: Tablero -> Int
cantE xs = sum [1 | x<-xs, x=='E']


-- | Verifica cada línea vertical, horizontal y diagonal en busca de un ganador.
-- Si lo encuentra devuelve el ganador (X u O), sino devuelve caracter vacío
ganador :: Tablero -> Char
ganador t
  -- Líneas horizontales
  | (t !! 0) /= 'E' && ((t !! 0) == (t !! 1) && (t !! 0) == (t !! 2)) = t !! 0
  | (t !! 3) /= 'E' && ((t !! 3) == (t !! 4) && (t !! 3) == (t !! 5)) = t !! 3
  | (t !! 6) /= 'E' && ((t !! 6) == (t !! 7) && (t !! 6) == (t !! 8)) = t !! 6
  -- Líneas verticales
  | (t !! 0) /= 'E' && ((t !! 0) == (t !! 3) && (t !! 0) == (t !! 6)) = t !! 0
  | (t !! 1) /= 'E' && ((t !! 1) == (t !! 4) && (t !! 1) == (t !! 7)) = t !! 1
  | (t !! 2) /= 'E' && ((t !! 2) == (t !! 5) && (t !! 2) == (t !! 8)) = t !! 2
  -- Líneas diagonales
  | (t !! 0) /= 'E' && ((t !! 0) == (t !! 4) && (t !! 0) == (t !! 8)) = t !! 0
  | (t !! 2) /= 'E' && ((t !! 2) == (t !! 4) && (t !! 2) == (t !! 6)) = t !! 2
  -- No hay ganador
  | otherwise = ' '


-- | Define que jugador tiene el siguiente turno usando __'cantX'__ y __'cantO'__
--
-- definiendo como siguiente jugador el que tenga un movimiento menos
proxJugador :: Tablero -> Char
proxJugador tablero
  | cantX tablero < cantO tablero = 'X'
  | otherwise = 'O'


-- | Arma una lista con todas posiciones vacías.
--
-- Invoca a __'ganador'__ y __'esValido'__
movPermitidos :: Tablero -> [Int]
movPermitidos tablero
  | (ganador tablero) /= ' ' = []
  | otherwise = [y | y <- [0..8], (esValido tablero y)]


-- | Retorna verdadero si el movimiento es válido
esValido :: Tablero -> Int -> Bool
esValido tablero p
  | p < 0 || p >= 9           = False   -- out of range
  | tablero !! p == 'E'       = True    -- empty
  | otherwise                 = False   -- played


-- | Esta función realiza el movimiento y devuelve un nuevo tablero
--
-- La función dentro de su definición se llama a si misma. 
mover :: Tablero -> Char -> Int -> Tablero
mover (p:tablero) ch pos
  | pos > 0 = p:[] ++ (mover tablero ch (pos - 1))
  | otherwise = ch:[] ++ tablero


-- | Dado un tablero elige una posición que asegura no se pierde
mejorMovimiento :: Tablero -> Int
mejorMovimiento tablero = movimiento
  where
  scored = scoreMoves tablero
  (movimiento, score) = foldr maxScore (head scored) (tail scored)


-- | Retorna una lista de tuplas (movimiento, puntaje)
scoreMoves :: Tablero -> [(Int, Int)]
scoreMoves tablero = zip (movPermitidos tablero) scores
  where
  tableros = map (mover tablero 'O') (movPermitidos tablero)
  scores = map evaluateBoardMax tableros


-- | scores the board and returns maximum value move for the given board
evaluateBoardMax :: Tablero -> Int
evaluateBoardMax tablero
  | length (movPermitidos tablero) == 0    = scoreBoard tablero 'O'
  | otherwise = foldr min (head scores) (tail scores)
  where
  tableros = map (mover tablero 'X') (movPermitidos tablero)
  scores = map evaluateBoardMin tableros


-- | scores the board and returns minimum value move for the given board
evaluateBoardMin :: Tablero -> Int
evaluateBoardMin tablero
  | length (movPermitidos tablero) == 0    = scoreBoard tablero 'O'
  | otherwise = foldr max (head scores) (tail scores)
  where
  tableros = map (mover tablero 'O') (movPermitidos tablero)
  scores = map evaluateBoardMax tableros


-- | Asigna un puntaje
scoreBoard :: Tablero -> Char -> Int
scoreBoard board player
  | (ganador board) == ' '     = 0
  | (ganador board) == player  = 1
  | otherwise                 = -1


-- | De dos tuplas (movimiento, puntaje) devuelve la de mayor puntaje
maxScore :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxScore (m0, s0) (m1, s1)
  | s0 > s1 = (m0, s0)
  | otherwise = (m1, s1)
