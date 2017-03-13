--ghci> :main estado1

module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
      [file] -> do
        x <- readFile file
        putStr x
