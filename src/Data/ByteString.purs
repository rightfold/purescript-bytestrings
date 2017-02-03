module Data.ByteString
( Octet
, ByteString

, unsafeFreeze
, unsafeThaw

, empty
, singleton
, pack
, unpack

, cons
, snoc
, uncons
, unsnoc
, head
, tail
, last
, init
, length
, isEmpty

, fromString
, toString
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid)
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

--------------------------------------------------------------------------------

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

instance showByteString :: Show ByteString where
    show bs = "pack " <> show (unpack bs)

--------------------------------------------------------------------------------

-- | *O(1)* The result points directly into the buffer. Mutating the buffer
-- | afterwards results in undefined behavior.
unsafeFreeze :: Buffer -> ByteString
unsafeFreeze = ByteString

-- | *O(1)* The result points directly into the byte string. Mutating the
-- | buffer results in undefined behavior.
unsafeThaw :: ByteString -> Buffer
unsafeThaw (ByteString s) = s

--------------------------------------------------------------------------------

-- | *O(1)* The empty byte string.
empty :: ByteString
empty = pack []

-- | *O(1)* A byte string with a single byte.
singleton :: Octet -> ByteString
singleton = pack <<< pure

-- | *O(n)* A byte string with many bytes.
pack :: Array Octet -> ByteString
pack = unsafeFreeze <<< unsafePerformEff <<< Buffer.fromArray

-- | *O(n)* Get the bytes from a byte string.
unpack :: ByteString -> Array Octet
unpack = unsafePerformEff <<< Buffer.toArray <<< unsafeThaw

--------------------------------------------------------------------------------

-- | *O(n)* Prepend a byte.
cons :: Octet -> ByteString -> ByteString
cons b bs = singleton b <> bs

-- | *O(n)* Append a byte.
snoc :: ByteString -> Octet -> ByteString
snoc bs b = bs <> singleton b

-- | *O(n)* Unprepend a byte.
uncons :: ByteString -> Maybe {head :: Octet, tail :: ByteString}
uncons bs = Array.uncons (unpack bs)
            <#> case _ of {head, tail} -> {head, tail: pack tail}

-- | *O(n)* Unappend a byte.
unsnoc :: ByteString -> Maybe {init :: ByteString, last :: Octet}
unsnoc bs = Array.unsnoc (unpack bs)
            <#> case _ of {init, last} -> {init: pack init, last}

-- | *O(1)* Get the first byte.
head :: ByteString -> Maybe Octet
head = unsafePerformEff <<< realGetAtOffset 0 <<< unsafeThaw

-- | *O(n)* Get all but the first byte.
tail :: ByteString -> Maybe ByteString
tail = uncons >>> map _.tail

-- | *O(1)* Get the last byte.
last :: ByteString -> Maybe Octet
last bs = unsafePerformEff do
    size <- Buffer.size (unsafeThaw bs)
    realGetAtOffset (size - 1) (unsafeThaw bs)

-- | *O(n)* Get all but the last byte.
init :: ByteString -> Maybe ByteString
init = unsnoc >>> map _.init

-- | *O(n)* How many bytes are in this byte string?
length :: ByteString -> Int
length = unsafePerformEff <<< Buffer.size <<< unsafeThaw

-- | *O(1)* Check if a byte string is empty.
isEmpty :: ByteString -> Boolean
isEmpty = length >>> eq 0

-- https://github.com/purescript-node/purescript-node-buffer/issues/14
foreign import realGetAtOffset
    :: ∀ eff. Int -> Buffer -> Eff (buffer :: BUFFER | eff) (Maybe Octet)

--------------------------------------------------------------------------------

-- | *O(n)* Encode a string.
fromString :: String -> Encoding -> ByteString
fromString s e = unsafeFreeze $ unsafePerformEff $ Buffer.fromString s e

-- | *O(n)* Decode a string.
toString :: ByteString -> Encoding -> String
toString s e = unsafePerformEff $ Buffer.toString e (unsafeThaw s)
