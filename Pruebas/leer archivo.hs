-- cat file | ./leer línea
-- parse error debido al espacio entre líneas

import Control.Monad
import Data.Char

-- Esta función quita del string pasado como argumento los saltos de línea. 
cadena :: String -> String
cadena xs = [x | x <- xs, x/='\n']

main :: IO ()
main = do
    -- Lee contenido de un archivo y lo guarda en l
    l <- getContents
    -- Imprime lo que la función cadena retorna más un salto de línea
    let a = cadena l
    b <- return "\n"
    putStr $ a ++ b
