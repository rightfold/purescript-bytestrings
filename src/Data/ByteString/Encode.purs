module Data.ByteString.Encode
  ( int8be
  , int8le
  , int16be
  , int16le
  , int32be
  , int32le

  , uint8be
  , uint8le
  , uint16be
  , uint16le
  ) where

import Data.ByteString (ByteString, Octet, pack)
import Data.Int.Bits ((.&.), shr, zshr)
import Prelude
import Unsafe.Coerce (unsafeCoerce)

pack' :: Array Int -> ByteString
pack' = pack <<< (unsafeCoerce :: Array Int -> Array Octet)

-- | Encode an integer of 8 bits in big-endian encoding.
-- | Extra bits will be discarded.
int8be :: Int -> ByteString
int8be i = pack' [(i `shr` 0) .&. 0xFF]

-- | Encode an integer of 16 bits in big-endian encoding.
-- | Extra bits will be discarded.
int16be :: Int -> ByteString
int16be i = pack' [(i `shr` 8) .&. 0xFF, (i `shr` 0) .&. 0xFF]

-- | Encode an integer of 32 bits in big-endian encoding.
int32be :: Int -> ByteString
int32be i = pack' [(i `shr` 24) .&. 0xFF, (i `shr` 16) .&. 0xFF, (i `shr` 8) .&. 0xFF, (i `shr` 0) .&. 0xFF]

-- | Encode an integer of 8 bits in little-endian encoding.
-- | Extra bits will be discarded.
int8le :: Int -> ByteString
int8le i = pack' [(i `shr` 0) .&. 0xFF]

-- | Encode an integer of 16 bits in little-endian encoding.
-- | Extra bits will be discarded.
int16le :: Int -> ByteString
int16le i = pack' [(i `shr` 0) .&. 0xFF, (i `shr` 8) .&. 0xFF]

-- | Encode an integer of 32 bits in little-endian encoding.
int32le :: Int -> ByteString
int32le i = pack' [(i `shr` 0) .&. 0xFF, (i `shr` 8) .&. 0xFF, (i `shr` 16) .&. 0xFF, (i `shr` 24) .&. 0xFF]

-- | Encode an integer of 8 bits in big-endian encoding.
-- | Extra bits will be discarded.
uint8be :: Int -> ByteString
uint8be i = pack' [(i `zshr` 0) .&. 0xFF]

-- | Encode an integer of 16 bits in big-endian encoding.
-- | Extra bits will be discarded.
uint16be :: Int -> ByteString
uint16be i = pack' [(i `zshr` 8) .&. 0xFF, (i `zshr` 0) .&. 0xFF]

-- | Encode an integer of 8 bits in little-endian encoding.
-- | Extra bits will be discarded.
uint8le :: Int -> ByteString
uint8le i = pack' [(i `zshr` 0) .&. 0xFF]

-- | Encode an integer of 16 bits in little-endian encoding.
-- | Extra bits will be discarded.
uint16le :: Int -> ByteString
uint16le i = pack' [(i `zshr` 0) .&. 0xFF, (i `zshr` 8) .&. 0xFF]
