{-# LANGUAGE Haskell98 #-}
-- cat file | ./leer línea
-- parse error debido al espacio entre líneas

-- | main
module Main where

import Control.Monad
import Data.Char

-- Esta función quita del string pasado como argumento los saltos de línea.

-- | as
cadena :: String -> String
cadena xs = [x | x <- xs, x/='\n']

-- | as
cantX :: String -> Int
cantX xs = sum [1 | x<-xs, x=='X']

-- | sg
cantO :: String -> Int
cantO xs = sum [1 | x<-xs, x=='O']

-- | agas
cantE :: String -> Int
cantE xs = sum [1 | x<-xs, x=='E']

-- | asg
main :: IO ()
main = do
    --  Lee contenido de un archivo y lo guarda en l
    l <- getContents
    --  Imprime lo que la función cadena retorna más un salto de línea
    let a = cadena l
    putStr $ a ++ "\n"
