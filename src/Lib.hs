{-# LANGUAGE OverloadedStrings #-}

module Lib (display, decode) where
import           Control.Monad
import           Data.Char
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Base64 as Base64
import           Data.List              (intersperse)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Encoding     as E
import           System.Console.Pretty (Color (..),  color, Pretty)

type Kind    = B.ByteString
data Identifier = Name B.ByteString | Id Integer
data RawPathElement = RawName B.ByteString | RawId Integer
type RawPath = [RawPathElement]

data Key = Key { kind :: Kind
               , identifier :: Identifier
               , parent :: Maybe Key
               , namespace :: Maybe B.ByteString
               }
type KeyPath = [(B.ByteString, B.ByteString)]

appendMissingChar :: B.ByteString -> B.ByteString
appendMissingChar input = if modulo == 0 then input else B.append input (C.replicate (4 - modulo) '=')
    where len    = B.length input
          modulo = len `mod` 4

splitBy :: B.ByteString -> [B.ByteString]
splitBy = join . map (C.split (chr 31)) . C.split (chr 30)

constructPath :: B.ByteString -> Either String RawPath
constructPath input = foldl addPath (Right []) path_
    where path_ = map (C.split (chr 31)) . C.split (chr 30) $ input
          addPath eitherPath parts =
            case eitherPath of
                Left msg -> Left msg
                Right path -> case parts of
                    [kind'] -> Right $ RawName kind' : path
                    -- TODO: use readMaybe
                    [kind', id'] -> Right $ RawName kind' : RawId (read . C.unpack $ id') : path
                    _ -> Left "unexpected input"

extractNameSpace :: RawPath -> Either String (RawPath, Maybe B.ByteString)
extractNameSpace path = if odd (length  path) then (oddCase path (last path)) else Right (path, Nothing)
    where oddCase path last' = case last' of
                                 RawName name -> Right (path, Just name)
                                 _ -> Left "unexpected input"

pairUp' :: (RawPath, Maybe B.ByteString) -> Either String Key
pairUp' (path, ns) = undefined

decode :: B.ByteString -> Either String KeyPath
decode = (pairUp =<<) . (splitBy <$>) . Base64.decode . appendMissingChar

pairUp :: [B.ByteString] -> Either String KeyPath
pairUp [] = Right []
pairUp (kind: name: rest) = ((kind, name) :) <$> pairUp rest
pairUp _ = Left "unmatch kind name"


keyPathToPrettyText :: KeyPath -> T.Text
keyPathToPrettyText paths = color Default "KEY(" `T.append`
                            (T.intercalate ", " $ foldl (\ ls (kind, name) -> ls ++ [color Blue (E.decodeUtf8 kind), color Green (E.decodeUtf8 name)]) [] paths)
                            `T.append` color Default ")"

displayKeyPath :: KeyPath -> IO ()
displayKeyPath = TIO.putStrLn . keyPathToPrettyText

display :: Either String KeyPath -> IO ()
display (Left err) = putStrLn (color Red err)
display (Right keyPath) = displayKeyPath keyPath