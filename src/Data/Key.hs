module Data.Key
            (Key(..),
             Kind,
             Identifier(..)) where

import qualified Data.ByteString        as B

type Kind       = B.ByteString
data Identifier = Name B.ByteString | Id Integer deriving (Show)

data Key = Key { kind :: Kind
               , identifier :: Identifier
               , parent :: Maybe Key
               , namespace :: Maybe B.ByteString
               } deriving (Show)