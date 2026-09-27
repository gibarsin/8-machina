module Keyboard where

import Data.Char (isHexDigit, digitToInt, toUpper)
import qualified Data.Map as Map
import Data.Word

import qualified Chip8.Keypad as Keypad
import Frontend (Input (..), PhysicalKey (..), keyLabel)

-- Which CHIP-8 key (0x0 to 0xF) each physical key presses.
type KeyMapping = Map.Map PhysicalKey Word8

-- The left side of a QWERTY keyboard, in the shape of the CHIP-8 keypad:
--   1 2 3 4        1 2 3 C
--   Q W E R        4 5 6 D
--   A S D F   ->   7 8 9 E
--   Z X C V        A 0 B F
defaultKeyMapping :: KeyMapping
defaultKeyMapping = Map.fromList
  [ (CharKey '1', 0x1), (CharKey '2', 0x2), (CharKey '3', 0x3), (CharKey '4', 0xc)
  , (CharKey 'Q', 0x4), (CharKey 'W', 0x5), (CharKey 'E', 0x6), (CharKey 'R', 0xd)
  , (CharKey 'A', 0x7), (CharKey 'S', 0x8), (CharKey 'D', 0x9), (CharKey 'F', 0xe)
  , (CharKey 'Z', 0xa), (CharKey 'X', 0x0), (CharKey 'C', 0xb), (CharKey 'V', 0xf)
  ]

-- Names accepted by --key, compared without case.
keyNames :: [(String, PhysicalKey)]
keyNames =
  [ ([character], CharKey character) | character <- ['A' .. 'Z'] ++ ['0' .. '9'] ]
  ++ [ ("KEYPAD" ++ show digit, KeypadKey digit) | digit <- [0 .. 9 :: Int] ]
  ++ [ ("UP", ArrowUp), ("DOWN", ArrowDown), ("LEFT", ArrowLeft), ("RIGHT", ArrowRight)
     , ("SPACE", SpaceKey), ("ENTER", EnterKey), ("RETURN", EnterKey)
     , ("TAB", TabKey), ("BACKSPACE", BackspaceKey)
     , ("LEFTSHIFT", LeftShiftKey), ("RIGHTSHIFT", RightShiftKey)
     , ("LEFTCTRL", LeftCtrlKey), ("RIGHTCTRL", RightCtrlKey)
     ]

-- Reads a NAME=HEX binding such as "Left=4".
readKeyBinding :: String -> Either String (PhysicalKey, Word8)
readKeyBinding text = case break (== '=') text of
  (name, '=' : [hexDigit])
    | Just key <- lookup (map toUpper name) keyNames
    , isHexDigit hexDigit -> Right (key, fromIntegral (digitToInt hexDigit))
  _ -> Left $ "Invalid key binding " ++ show text
         ++ ", expected NAME=HEX like Left=4, where NAME is a letter, digit, Keypad0-Keypad9, "
         ++ "Up, Down, Left, Right, Space, Enter, Tab, Backspace, LeftShift, RightShift, LeftCtrl or RightCtrl"

-- Adds the bindings to the default layout; the default keys keep working.
keyMappingWith :: [(PhysicalKey, Word8)] -> KeyMapping
keyMappingWith bindings = Map.union (Map.fromList bindings) defaultKeyMapping

-- The keypad after this frame's key presses and releases.
updateKeypad :: KeyMapping -> [Input] -> Keypad.Keypad -> Keypad.Keypad
updateKeypad mapping inputs current = foldl apply current inputs
  where
    apply keys (KeyDown key) = maybe keys (\n -> Keypad.setKey n True keys) (Map.lookup key mapping)
    apply keys (KeyUp key)   = maybe keys (\n -> Keypad.setKey n False keys) (Map.lookup key mapping)
    apply keys QuitRequested = keys

-- The name of every key pressed this frame that has no CHIP-8 mapping.
unmappedKeyNames :: KeyMapping -> [Input] -> [String]
unmappedKeyNames mapping inputs =
  [ keyLabel key | KeyDown key <- inputs, Map.notMember key mapping ]
