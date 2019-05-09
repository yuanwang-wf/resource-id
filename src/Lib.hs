{-# LANGUAGE OverloadedStrings #-}

module Lib (display, decode) where
import           Control.Monad
import           Data.Char
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.IO           as T
import qualified Data.Text.Encoding     as E
import           System.Console.Pretty (Color (..),  color)

type Kind    = B.ByteString
type KeyPath = [(B.ByteString, B.ByteString)]

appendMissingChar :: B.ByteString -> B.ByteString
appendMissingChar input = if modulo == 0 then input else B.append input (C.replicate (4 - modulo) '=')
    where len    = B.length input
          modulo = len `mod` 4

splitBy :: B.ByteString -> [B.ByteString]
splitBy = join . map (C.split (chr 31)) . C.split (chr 30)

decode :: B.ByteString -> Either String KeyPath
decode = (pairUp =<<) . (splitBy <$>) . Base64.decode . appendMissingChar

pairUp :: [B.ByteString] -> Either String KeyPath
pairUp [] = Right []
pairUp (kind: name: rest) = ((kind, name) :) <$> pairUp rest
pairUp _ = Left "unmatch kind name"

--TODO turn this into KeyPath -> [Pretty Text]
-- so we can add space and seprator correctly
displayKeyPath :: KeyPath -> IO ()
displayKeyPath paths = do
    T.putStr (color Default "KEY(")
    forM_ paths (\ (kind, name) -> T.putStr (color Blue (E.decodeUtf8 kind)) >> T.putStr ", " >> T.putStr (color Green (E.decodeUtf8 name)) >> T.putStr " ")
    T.putStrLn (color Default ")")

display :: Either String KeyPath -> IO ()
display (Left err) = putStrLn (color Red err)
display (Right keyPath) = displayKeyPath keyPath