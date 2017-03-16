--ghci> :main estado1

module Main where
import System.Environment
import Data.Char

main :: IO ()
main = do
    args <- getArgs
    case args of
      [file] -> do
        x <- readFile file
        putStr x
