module Keyboard where

import Control.Monad
import Data.Char (isHexDigit, digitToInt, ord, toLower, toUpper)
import qualified Data.Map as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Word
import qualified SDL
import SDL.Input.Keyboard
import System.IO (hPutStrLn, stderr)

import qualified Chip8.Keypad as Keypad

type Key = SDL.Keysym

-- Which CHIP-8 key (0x0 to 0xF) each keyboard key presses.
type KeyMapping = Map.Map SDL.Keycode Word8

-- The left side of a QWERTY keyboard, in the shape of the CHIP-8 keypad:
--   1 2 3 4        1 2 3 C
--   Q W E R        4 5 6 D
--   A S D F   ->   7 8 9 E
--   Z X C V        A 0 B F
defaultKeyMapping :: KeyMapping
defaultKeyMapping = Map.fromList
  [ (characterKeycode '1', 0x1), (characterKeycode '2', 0x2), (characterKeycode '3', 0x3), (characterKeycode '4', 0xc)
  , (characterKeycode 'Q', 0x4), (characterKeycode 'W', 0x5), (characterKeycode 'E', 0x6), (characterKeycode 'R', 0xd)
  , (characterKeycode 'A', 0x7), (characterKeycode 'S', 0x8), (characterKeycode 'D', 0x9), (characterKeycode 'F', 0xe)
  , (characterKeycode 'Z', 0xa), (characterKeycode 'X', 0x0), (characterKeycode 'C', 0xb), (characterKeycode 'V', 0xf)
  ]

-- SDL gives letter and digit keys the code of their lowercase character.
characterKeycode :: Char -> SDL.Keycode
characterKeycode character = SDL.Keycode (fromIntegral (ord (toLower character)))

-- Names accepted by --key, compared without case.
keyNames :: [(String, SDL.Keycode)]
keyNames =
  [ ([character], characterKeycode character) | character <- ['A' .. 'Z'] ++ ['0' .. '9'] ]
  ++ [ ("KEYPAD" ++ show digit, keypadKeycode) | (digit, keypadKeycode) <- zip [0 :: Int ..] keypadDigits ]
  ++ [ ("UP", SDL.KeycodeUp), ("DOWN", SDL.KeycodeDown), ("LEFT", SDL.KeycodeLeft), ("RIGHT", SDL.KeycodeRight)
     , ("SPACE", SDL.KeycodeSpace), ("ENTER", SDL.KeycodeReturn), ("RETURN", SDL.KeycodeReturn)
     , ("TAB", SDL.KeycodeTab), ("BACKSPACE", SDL.KeycodeBackspace)
     , ("LEFTSHIFT", SDL.KeycodeLShift), ("RIGHTSHIFT", SDL.KeycodeRShift)
     , ("LEFTCTRL", SDL.KeycodeLCtrl), ("RIGHTCTRL", SDL.KeycodeRCtrl)
     ]
  where
    keypadDigits =
      [ SDL.KeycodeKP0, SDL.KeycodeKP1, SDL.KeycodeKP2, SDL.KeycodeKP3, SDL.KeycodeKP4
      , SDL.KeycodeKP5, SDL.KeycodeKP6, SDL.KeycodeKP7, SDL.KeycodeKP8, SDL.KeycodeKP9 ]

-- Reads a NAME=HEX binding such as "Left=4".
readKeyBinding :: String -> Either String (SDL.Keycode, Word8)
readKeyBinding text = case break (== '=') text of
  (name, '=' : [hexDigit])
    | Just keycode <- lookup (map toUpper name) keyNames
    , isHexDigit hexDigit -> Right (keycode, fromIntegral (digitToInt hexDigit))
  _ -> Left $ "Invalid key binding " ++ show text
         ++ ", expected NAME=HEX like Left=4, where NAME is a letter, digit, Keypad0-Keypad9, "
         ++ "Up, Down, Left, Right, Space, Enter, Tab, Backspace, LeftShift, RightShift, LeftCtrl or RightCtrl"

-- Adds the bindings to the default layout; the default keys keep working.
keyMappingWith :: [(SDL.Keycode, Word8)] -> KeyMapping
keyMappingWith bindings = Map.union (Map.fromList bindings) defaultKeyMapping

-- SDL's human-readable name for the physical key, e.g. "Return" or "Space".
keyName :: Key -> IO String
keyName key = getScancodeName (keysymScancode key)

toKeyNumber :: KeyMapping -> (Key, Bool) -> Maybe (Word8, Bool)
toKeyNumber mapping (key, pressed) =
  fmap (\keyNumber -> (keyNumber, pressed)) (Map.lookup (keysymKeycode key) mapping)

-- The keypad after this frame's key presses and releases.
updateKeypad :: KeyMapping -> [(Key, Bool)] -> Keypad.Keypad -> Keypad.Keypad
updateKeypad mapping keyPresses current =
  foldl (\keys (keyNumber, pressed) -> Keypad.setKey keyNumber pressed keys) current
        (mapMaybe (toKeyNumber mapping) keyPresses)

logUnmappedKeys :: KeyMapping -> [(Key, Bool)] -> IO ()
logUnmappedKeys mapping keyPresses =
  forM_ keyPresses $ \(key, pressed) ->
    when (pressed && isNothing (Map.lookup (keysymKeycode key) mapping)) $ do
      name <- keyName key
      hPutStrLn stderr $ "Ignoring unmapped key: " ++ name
