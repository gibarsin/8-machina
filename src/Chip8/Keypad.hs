module Chip8.Keypad
  ( Keypad
  , noKeysPressed
  , setKey
  , isKeyPressed
  , firstPressedKey
  ) where

import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word8)

-- Which of the sixteen CHIP-8 keys, 0x0 to 0xF, are held down.
newtype Keypad = Keypad (Vector.Vector Bool)

noKeysPressed :: Keypad
noKeysPressed = Keypad (Vector.replicate 16 False)

setKey :: Word8 -> Bool -> Keypad -> Keypad
setKey key pressed (Keypad keys)
  | key < 16 = Keypad (keys Vector.// [(fromIntegral key, pressed)])
  | otherwise = Keypad keys

-- Keys above 0xF do not exist, so they are never pressed.
isKeyPressed :: Word8 -> Keypad -> Bool
isKeyPressed key (Keypad keys) = key < 16 && keys Vector.! fromIntegral key

firstPressedKey :: Keypad -> Maybe Word8
firstPressedKey (Keypad keys) = fromIntegral <$> Vector.findIndex id keys
