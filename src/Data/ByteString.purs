module Data.ByteString
( module Node.Encoding

, Octet
, ByteString

, unsafeFreeze
, unsafeThaw

, empty
, singleton
, pack
, unpack

, index
, (!!)
, unsafeIndex
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

, map
, reverse
, foldl
, foldr

, fromString
, toString
, toUTF8
, fromUTF8
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid)
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Prelude hiding (map)
import Prelude as Prelude
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
    show bs = "(pack " <> show (unpack bs) <> ")"

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

-- | *O(1)* Get the nth byte.
index :: ByteString -> Int -> Maybe Octet
index b i = unsafePerformEff $ realGetAtOffset i (unsafeThaw b)

infixl 8 index as !!

-- | *O(1)* Get the nth byte. If the index is out of bounds, the behavior is
-- | undefined.
foreign import unsafeIndex :: ByteString -> Int -> Octet

-- https://github.com/purescript-node/purescript-node-buffer/issues/14
foreign import realGetAtOffset
    :: ∀ eff. Int -> Buffer -> Eff (buffer :: BUFFER | eff) (Maybe Octet)

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
head = (_ !! 0)

-- | *O(n)* Get all but the first byte.
tail :: ByteString -> Maybe ByteString
tail = uncons >>> Prelude.map _.tail

-- | *O(1)* Get the last byte.
last :: ByteString -> Maybe Octet
last bs = bs !! (length bs - 1)

-- | *O(n)* Get all but the last byte.
init :: ByteString -> Maybe ByteString
init = unsnoc >>> Prelude.map _.init

-- | *O(1)* How many bytes are in this byte string?
length :: ByteString -> Int
length = unsafePerformEff <<< Buffer.size <<< unsafeThaw

-- | *O(1)* Check if a byte string is empty.
isEmpty :: ByteString -> Boolean
isEmpty = length >>> eq 0

--------------------------------------------------------------------------------

-- | *O(n)* Transform the bytes in the byte string.
map :: (Octet -> Octet) -> ByteString -> ByteString
map f = pack <<< Prelude.map f <<< unpack

-- | *O(n)* Reverse the byte string.
reverse :: ByteString -> ByteString
reverse = pack <<< Array.reverse <<< unpack

-- | *O(n)*
foreign import foldl :: ∀ a. (a -> Octet -> a) -> a -> ByteString -> a

-- | *O(n)*
foreign import foldr :: ∀ a. (Octet -> a -> a) -> a -> ByteString -> a

--------------------------------------------------------------------------------

-- | *O(n)* Encode a string.
fromString :: String -> Encoding -> ByteString
fromString s e = unsafeFreeze $ unsafePerformEff $ Buffer.fromString s e

-- | *O(n)* Decode a string.
toString :: ByteString -> Encoding -> String
toString s e = unsafePerformEff $ Buffer.toString e (unsafeThaw s)

-- | *O(n)* `flip fromString UTF8`.
toUTF8 :: String -> ByteString
toUTF8 = flip fromString UTF8

-- | *O(n)* `flip toString UTF8`
fromUTF8 :: ByteString -> String
fromUTF8 = flip toString UTF8
