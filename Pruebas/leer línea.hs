-- cat file | ./leer línea

import Control.Monad
import Data.Char

main = do
    --putStr "Give me some input: "
    l <- getContents
    --putStrLn $ map toUpper l
    putStr l
