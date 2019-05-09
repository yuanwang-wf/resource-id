module Main where

import qualified Data.ByteString.Char8  as C
import           Lib
import           System.Environment
import           System.Exit


work :: String -> IO ()
work = display . decode . C.pack

main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then work (head args) else die "incorrect argument"