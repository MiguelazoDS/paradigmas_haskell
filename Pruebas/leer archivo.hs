-- cat file | ./leer línea
-- parse error debido al espacio entre líneas

import Control.Monad
import Data.Char

function :: String -> String
function input =
            -- allLines es una lista que contiene tantos valores como lineas tenga el archivo
            let allLines = lines input
                shortLines = filter (\line -> length line < 10) allLines
                result = unlines shortLines
            in  result

main :: IO ()
main = do
    -- Lee contenido de un archivo y lo guarda en l
    l <- getContents
    -- Imprime lo que la función function retorna
    putStr $ function l
