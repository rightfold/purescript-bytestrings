module Data.ByteString
( Octet
, ByteString

, unsafeFreeze
, unsafeThaw

, empty

, unpack

, fromString
, toString
) where

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Monoid (class Monoid)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 255.
type Octet = Int

-- | A packed sequence of bytes.
newtype ByteString = ByteString Buffer

instance semigroupByteString :: Semigroup ByteString where
    append a b = unsafeFreeze $ unsafePerformEff $ Buffer.concat [unsafeThaw a, unsafeThaw b]

instance monoidByteString :: Monoid ByteString where
    mempty = empty

instance eqByteString :: Eq ByteString where
    eq a b = unpack a == unpack b

instance ordByteString :: Ord ByteString where
    compare a b = unpack a `compare` unpack b

instance arbitraryByteString :: Arbitrary ByteString where
    arbitrary = fromString `flip` UTF8 <$> arbitrary

-- | The result points directly into the buffer. Mutating the buffer afterwards
-- | results in undefined behavior.
unsafeFreeze :: Buffer -> ByteString
unsafeFreeze = ByteString

-- | The result points directly into the byte string. Mutating the buffer
-- | results in undefined behavior.
unsafeThaw :: ByteString -> Buffer
unsafeThaw (ByteString s) = s

-- | The empty byte string.
empty :: ByteString
empty = unsafeFreeze $ unsafePerformEff $ Buffer.create 0

-- | Return the bytes of a byte string.
unpack :: ByteString -> Array Octet
unpack = unsafePerformEff <<< Buffer.toArray <<< unsafeThaw

-- | Encode a string.
fromString :: String -> Encoding -> ByteString
fromString s e = unsafeFreeze $ unsafePerformEff $ Buffer.fromString s e

-- | Decode a string.
toString :: ByteString -> Encoding -> String
toString s e = unsafePerformEff $ Buffer.toString e (unsafeThaw s)
