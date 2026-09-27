module Chip8.Memory
  ( Memory
  , Address
  , memorySize
  , emptyMemory
  , readByte
  , readBytes
  , writeBytes
  ) where

import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word16, Word8)

type Address = Word16

-- The 4 KB of CHIP-8 memory. Addresses past the end wrap around.
newtype Memory = Memory (Vector.Vector Word8)

memorySize :: Int
memorySize = 4096

emptyMemory :: Memory
emptyMemory = Memory (Vector.replicate memorySize 0)

readByte :: Address -> Memory -> Word8
readByte address (Memory bytes) = bytes Vector.! wrap address

readBytes :: Address -> Int -> Memory -> [Word8]
readBytes address count memory =
  [ readByte (address + fromIntegral offset) memory | offset <- [0 .. count - 1] ]

writeBytes :: Address -> [Word8] -> Memory -> Memory
writeBytes address values (Memory bytes) =
  Memory (bytes Vector.// zip [ wrap (address + fromIntegral offset) | offset <- [0 :: Int ..] ] values)

wrap :: Address -> Int
wrap address = fromIntegral address `mod` memorySize
