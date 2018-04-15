module Keyboard where

import Control.Monad
import Data.Array.IO
import Data.Word
import qualified SDL
import SDL.Input.Keyboard

type Keypad = IOUArray Word8 Bool

type Key = SDL.Keycode

createKeypad :: IO Keypad
createKeypad = newArray (0, 15) False

mergeKeypad :: Keypad -> [(Key, Bool)] -> IO ()
mergeKeypad keypad keypadUpdate =
  mergeKeypad' keypad $ map toKeyNumber keypadUpdate

mergeKeypad' :: Keypad -> [(Word8, Bool)] -> IO ()
mergeKeypad' keypad keypadUpdate =
  forM_ keypadUpdate $ \(index, value) -> writeArray keypad index value

keyMapping :: SDL.Keycode -> Word8

keyMapping SDL.Keycode1 = 0x1
keyMapping SDL.Keycode2 = 0x2
keyMapping SDL.Keycode3 = 0x3
keyMapping SDL.KeycodeQ = 0x4
keyMapping SDL.KeycodeW = 0x5
keyMapping SDL.KeycodeE = 0x6
keyMapping SDL.KeycodeA = 0x7
keyMapping SDL.KeycodeS = 0x8
keyMapping SDL.KeycodeD = 0x9
keyMapping SDL.KeycodeX = 0x0
keyMapping SDL.KeycodeZ = 0xa
keyMapping SDL.KeycodeC = 0xb
keyMapping SDL.Keycode4 = 0xc
keyMapping SDL.KeycodeR = 0xd
keyMapping SDL.KeycodeF = 0xe
keyMapping SDL.KeycodeV = 0xf

toKeyNumber :: (Key, Bool) -> (Word8, Bool)
toKeyNumber (keycode, pressed)  = (keyMapping keycode, pressed)

isKeyPressed :: Keypad -> Word8 -> IO Bool
isKeyPressed keypad keyword = readArray keypad keyword
