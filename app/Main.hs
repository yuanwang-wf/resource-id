{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import qualified Data.ByteString.Char8  as C
-- import           Lib
-- import           System.Environment
-- import           System.Exit
import Data.Key
import Data.KeyEncode

import qualified Data.ByteString        as B

-- work :: String -> IO ()
-- work = display . decode . C.pack

-- main :: IO ()
-- main = do
--     args <- getArgs
--     if length args == 1 then work (head args) else die "incorrect argument"
input :: B.ByteString
input = "V0ZEYXRhRW50aXR5HlByb2plY3Q6NjY2NzU4MmQ0OGM3NDMyZmEwODVlNWYyMGUxODY4NTE"
input2 :: B.ByteString
input2 = "S2luZDEeMR5LaW5kMh8y"
main = print (encode "S2luZDEeMR5LaW5kMh8y") >> print (encode input)