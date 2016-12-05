module Data.ByteString
( ByteString

, unsafeFreeze
, unsafeThaw

, fromString
) where

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding)
import Prelude

-- | A packed sequence of bytes.
newtype ByteString = ByteString Buffer

-- | The result points directly into the buffer. Mutating the buffer afterwards
-- | results in undefined behavior.
unsafeFreeze :: Buffer -> ByteString
unsafeFreeze = ByteString

-- | The result points directly into the byte string. Mutating the buffer
-- | results in undefined behavior.
unsafeThaw :: ByteString -> Buffer
unsafeThaw (ByteString s) = s

-- | Encode a string.
fromString :: String -> Encoding -> ByteString
fromString s e = unsafeFreeze $ unsafePerformEff $ Buffer.fromString s e
