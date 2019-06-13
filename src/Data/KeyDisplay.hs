{-# LANGUAGE OverloadedStrings #-}
module Data.KeyDisplay (displayKey) where

import           Data.Key
import qualified Data.ByteString.Char8  as C
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import           System.Console.Pretty (Color (..),  color, Pretty)

kindToPrettyText :: Kind -> T.Text
kindToPrettyText = color Blue . E.decodeUtf8

idToPrettyText :: Identifier -> T.Text
idToPrettyText (Name n) =  color Green "\"" <> color Green (E.decodeUtf8 n) <> color Green "\""
idToPrettyText (Id val) = color Yellow (E.decodeUtf8 . C.pack . show $ val)

toPrettyText :: Key -> T.Text
toPrettyText key' = case parent key' of
    Nothing -> kindToPrettyText (kind key') <> ", " <> (idToPrettyText (identifier key'))
    Just p' -> toPrettyText p' <> ", " <> kindToPrettyText (kind key') <> ", " <> (idToPrettyText (identifier key'))


displayKey :: Key -> T.Text
displayKey key = color Default "KEY(" <> toPrettyText key <> color Default ")"
