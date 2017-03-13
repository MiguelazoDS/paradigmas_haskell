--import Control.Monad
import Data.Char
import System.IO

main :: IO ()
main = do
      theFile <- openFile "estado1" ReadMode
      contents <- hGetContents theFile
      putStrLn contents
      hClose theFile
