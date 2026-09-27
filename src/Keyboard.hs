module Keyboard where

import Control.Monad
import Data.Array.IO
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Word
import qualified SDL
import SDL.Input.Keyboard
import System.IO (hPutStrLn, stderr)

type Keypad = IOUArray Word8 Bool

type Key = SDL.Keysym

createKeypad :: IO Keypad
createKeypad = newArray (0, 15) False

mergeKeypad :: Keypad -> [(Key, Bool)] -> IO ()
mergeKeypad keypad keypadUpdate = do
  forM_ unmappedPresses $ \(key, _) -> do
    name <- keyName key
    hPutStrLn stderr $ "Ignoring unmapped key: " ++ name
  mergeKeypad' keypad $ mapMaybe toKeyNumber keypadUpdate
  where
    unmappedPresses =
      filter (\(key, pressed) -> pressed && isNothing (keyMapping (keysymKeycode key))) keypadUpdate

mergeKeypad' :: Keypad -> [(Word8, Bool)] -> IO ()
mergeKeypad' keypad keypadUpdate =
  forM_ keypadUpdate $ \(index, value) -> writeArray keypad index value

keyMapping :: SDL.Keycode -> Maybe Word8

keyMapping SDL.Keycode1 = Just 0x1
keyMapping SDL.Keycode2 = Just 0x2
keyMapping SDL.Keycode3 = Just 0x3
keyMapping SDL.KeycodeQ = Just 0x4
keyMapping SDL.KeycodeW = Just 0x5
keyMapping SDL.KeycodeE = Just 0x6
keyMapping SDL.KeycodeA = Just 0x7
keyMapping SDL.KeycodeS = Just 0x8
keyMapping SDL.KeycodeD = Just 0x9
keyMapping SDL.KeycodeX = Just 0x0
keyMapping SDL.KeycodeZ = Just 0xa
keyMapping SDL.KeycodeC = Just 0xb
keyMapping SDL.Keycode4 = Just 0xc
keyMapping SDL.KeycodeR = Just 0xd
keyMapping SDL.KeycodeF = Just 0xe
keyMapping SDL.KeycodeV = Just 0xf
keyMapping _            = Nothing

-- SDL's human-readable name for the physical key, e.g. "Return" or "Space".
keyName :: Key -> IO String
keyName key = getScancodeName (keysymScancode key)

toKeyNumber :: (Key, Bool) -> Maybe (Word8, Bool)
toKeyNumber (key, pressed) = fmap (\keyNumber -> (keyNumber, pressed)) (keyMapping (keysymKeycode key))

isKeyPressed :: Keypad -> Word8 -> IO Bool
isKeyPressed keypad keyword = readArray keypad keyword

findPressedKey :: Keypad -> IO (Maybe Word8)
findPressedKey keypad = do
  pressedKeys <- filterM (isKeyPressed keypad) [0x0 .. 0xF]
  return (listToMaybe pressedKeys)
