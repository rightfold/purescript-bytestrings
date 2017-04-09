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

, Foldable
, foldableOfOctet
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
import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.Leibniz (type (~), Leibniz(..), coerceSymm)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid)
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Prelude hiding (map)
import Prelude as Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 256.
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

-- | *Θ(n)* A byte string with many bytes.
pack :: Array Octet -> ByteString
pack = unsafeFreeze <<< unsafePerformEff <<< Buffer.fromArray

-- | *Θ(n)* Get the bytes from a byte string.
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

-- | *Θ(n)* Prepend a byte.
cons :: Octet -> ByteString -> ByteString
cons b bs = singleton b <> bs

-- | *Θ(n)* Append a byte.
snoc :: ByteString -> Octet -> ByteString
snoc bs b = bs <> singleton b

-- | *Θ(n)* Unprepend a byte.
uncons :: ByteString -> Maybe {head :: Octet, tail :: ByteString}
uncons bs = Array.uncons (unpack bs)
            <#> case _ of {head: h, tail: t} -> {head: h, tail: pack t}

-- | *Θ(n)* Unappend a byte.
unsnoc :: ByteString -> Maybe {init :: ByteString, last :: Octet}
unsnoc bs = Array.unsnoc (unpack bs)
            <#> case _ of {init: i, last: l} -> {init: pack i, last: l}

-- | *O(1)* Get the first byte.
head :: ByteString -> Maybe Octet
head = (_ !! 0)

-- | *Θ(n)* Get all but the first byte.
tail :: ByteString -> Maybe ByteString
tail = uncons >>> Prelude.map _.tail

-- | *O(1)* Get the last byte.
last :: ByteString -> Maybe Octet
last bs = bs !! (length bs - 1)

-- | *Θ(n)* Get all but the last byte.
init :: ByteString -> Maybe ByteString
init = unsnoc >>> Prelude.map _.init

-- | *O(1)* How many bytes are in this byte string?
length :: ByteString -> Int
length = unsafePerformEff <<< Buffer.size <<< unsafeThaw

-- | *O(1)* Check if a byte string is empty.
isEmpty :: ByteString -> Boolean
isEmpty = length >>> eq 0

--------------------------------------------------------------------------------

-- | *Θ(n)* Transform the bytes in the byte string.
map :: (Octet -> Octet) -> ByteString -> ByteString
map f = pack <<< Prelude.map f <<< unpack

-- | *Θ(n)* Reverse the byte string.
reverse :: ByteString -> ByteString
reverse = pack <<< Array.reverse <<< unpack

--------------------------------------------------------------------------------

-- | A foldable byte string.
newtype Foldable a = Foldable ByteString

derive instance newtypeFoldable :: Newtype (Foldable Int) _

instance foldableFoldable :: Foldable Foldable where
    foldMap = foldMapDefaultL
    foldl f z fb@(Foldable b) = foldl f' z b
        where f' x o = f x (coerceSymm leibniz o)
              leibniz = foldableOfOctet fb
    foldr f z fb@(Foldable b) = foldr f' z b
        where f' o x = f (coerceSymm leibniz o) x
              leibniz = foldableOfOctet fb

-- | *O(1)* Witness that foldable byte strings can only contain octets.
foldableOfOctet :: ∀ a. Foldable a -> a ~ Octet
foldableOfOctet = const $ Leibniz unsafeCoerce

-- | *Θ(n)* Fold a byte string.
foreign import foldl :: ∀ a. (a -> Octet -> a) -> a -> ByteString -> a

-- | *Θ(n)* Fold a byte string.
foreign import foldr :: ∀ a. (Octet -> a -> a) -> a -> ByteString -> a

--------------------------------------------------------------------------------

-- | *Θ(n)* Encode a string.
fromString :: String -> Encoding -> ByteString
fromString s e = unsafeFreeze $ unsafePerformEff $ Buffer.fromString s e

-- | *Θ(n)* Decode a string.
toString :: ByteString -> Encoding -> String
toString s e = unsafePerformEff $ Buffer.toString e (unsafeThaw s)

-- | *Θ(n)* `flip fromString UTF8`.
toUTF8 :: String -> ByteString
toUTF8 = flip fromString UTF8

-- | *Θ(n)* `flip toString UTF8`
fromUTF8 :: ByteString -> String
fromUTF8 = flip toString UTF8
