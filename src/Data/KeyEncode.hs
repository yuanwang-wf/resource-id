module Data.KeyEncode where

import           Data.Char
import           Data.Key
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as C

data KeyPath' = Empty | Element {kind' :: Kind, id' :: Maybe Identifier, parent' :: KeyPath'}

appendKeyPath :: B.ByteString -> KeyPath' -> KeyPath'
appendKeyPath part path = case path of
    Empty -> Element part Nothing Empty
    Element k' Nothing p' -> Element k' (Just $ Name part) p'
    Element k' i' p' -> Element part Nothing (Element k' i' p')

appendKeyId :: B.ByteString -> Integer -> KeyPath' -> KeyPath'
appendKeyId k' i' = Element k' (Just $ Id i')


toKey :: KeyPath' -> Maybe Key
toKey Empty = Nothing
toKey (Element k' Nothing p') = Nothing
toKey (Element k' (Just i') p') = Just $ Key k' i' (toKey p') Nothing

roundUpChar :: B.ByteString -> B.ByteString
roundUpChar input = if modulo == 0 then input else B.append input (C.replicate (4 - modulo) '=')
    where len    = B.length input
          modulo = len `mod` 4

firstSplit :: B.ByteString -> [B.ByteString]
firstSplit = C.split (chr 30)

--TODO use readMaybe, and handle other case in C.split
secondSplit :: [B.ByteString] -> KeyPath'
secondSplit = foldl parseId Empty
    where parseId path part = case C.split (chr 31) part of
                                [k'] -> appendKeyPath k' path
                                [k', i'] -> appendKeyId k' (read . C.unpack $ i')  path


decode' :: B.ByteString -> Maybe Key
decode' input = case toKey path of
                    Nothing -> Nothing
                    Just key -> Just $ Key (kind key) (identifier key) (parent key) Nothing
    where path = secondSplit . firstSplit $ input


encode :: B.ByteString -> Maybe Key
encode = either (const Nothing) decode' . Base64.decode . roundUpChar

encodeI = Base64.decode . roundUpChar