{-# LANGUAGE Haskell98 #-}
-- cat file | ./leer línea
-- parse error debido al espacio entre líneas

module Main where

import Control.Monad
import Data.Char

type Tablero = String
-- Esta función quita del string pasado como argumento los saltos de línea.

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


-- | Omite los espacios y saltos de línea que se leyeron del archivo
unaLinea :: Tablero -> String
unaLinea xs = [x | x <- xs, x/='\n', x/=' ']

-- Retorna verdadero si el movimiento es válido
esValido :: Tablero -> Int -> Bool
esValido tablero p
  | p < 0 || p >= 9           = False   -- out of range
  | tablero !! p == 'E'       = True    -- empty
  | otherwise                 = False   -- played

movPermitidos :: Tablero -> [Int]
movPermitidos tablero
  | (ganador tablero) /= ' ' = []
  | otherwise = [y | y <- [0..8], (esValido tablero y)]

-- | Verifica cada línea vertical, horizontal y diagonal en busca de un ganador
-- | Si lo encuentra devuelve el ganador (X u O), sino devuelve caracter vacío
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

-- | Verifica que el formato del estado de juego sea válido
formato :: Tablero -> Bool
formato tablero
  | x == 3 && o == 3 = False
  | x  >= 4 || o >=4 = False
  | l > 0 = False
  | otherwise = True
  where
  (x,o)=(cantX tablero, cantO tablero)
  l = sum [1 | x<-tablero, x/='O', x/='X', x/='E']

-- | Define que figura se juega.
prox_jugador :: Tablero -> Char
prox_jugador tablero
  | cantX tablero > cantO tablero = 'X'
  | otherwise = 'O'

-- | Cuenta la cantidad de 'X'
cantX :: Tablero -> Int
cantX xs = sum [1 | x<-xs, x=='X']

-- | Cuenta la cantidad de 'O'
cantO :: Tablero -> Int
cantO xs = sum [1 | x<-xs, x=='O']

-- mover
-- Given a tablero, a player and a mover position, returns a new tablero with the
-- new mover applied
mover :: Tablero -> Char -> Int -> Tablero
mover (p:tablero) ch pos
  | pos > 0 = p:[] ++ (mover tablero ch (pos - 1))
  | otherwise = ch:[] ++ tablero

-- | Empieza el juego con es estado recibido desde main
juego :: String -> IO()
juego tablero = do
  putStr $ show $ movPermitidos tablero
  putStr $ show $ prox_jugador tablero
  putStr "\n"
  putStr $ mostrarTablero $ mover tablero (prox_jugador tablero) 0
  if not $ formato tablero
    then error "Formato inválido"
    else putStr "Formato válido\n"


-- | Función principal
main :: IO ()
main = do
    --  Lee contenido de un archivo y lo guarda en cadena
    cadena <- getContents
    let tablero = unaLinea cadena
    --  Imprime el tablero
    putStr $ mostrarTablero tablero
    juego tablero
