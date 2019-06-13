{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.KeyEncode
import           System.Environment
import           System.Exit


main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then displayEncode (head args) else die "incorrect argument"
