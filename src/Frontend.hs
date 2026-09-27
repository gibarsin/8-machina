module Frontend where

import Data.Word (Word8)

import Chip8.VideoMemory (Screen)

-- Red, green and blue components.
type Color = (Word8, Word8, Word8)

data Colors = Colors
  { foregroundColor :: Color
  , backgroundColor :: Color
  }

-- The only thing the emulation loop knows about a user interface.
data Frontend = Frontend
  { pollInput :: IO [Input]
  , present   :: Screen -> IO ()
  , setBeep   :: Bool -> IO ()
  , close     :: IO ()
  }

data Input = KeyDown PhysicalKey | KeyUp PhysicalKey | QuitRequested
  deriving (Eq)

-- A key identity independent of any particular UI toolkit.
data PhysicalKey
  = CharKey Char                -- a letter or digit, e.g. CharKey 'Q'
  | KeypadKey Int                -- the numeric keypad, 0 to 9
  | ArrowUp | ArrowDown | ArrowLeft | ArrowRight
  | SpaceKey | EnterKey | TabKey | BackspaceKey
  | LeftShiftKey | RightShiftKey | LeftCtrlKey | RightCtrlKey
  | OtherKey String              -- any other key, named by the front-end
  deriving (Eq, Ord)

-- The name shown to the user, e.g. in the unmapped-key log. These match
-- SDL's own getScancodeName strings exactly, so the log reads the same
-- as it did before front-ends were abstracted out.
keyLabel :: PhysicalKey -> String
keyLabel (CharKey c) = [c]
keyLabel (KeypadKey n) = "Keypad " ++ show n
keyLabel ArrowUp = "Up"
keyLabel ArrowDown = "Down"
keyLabel ArrowLeft = "Left"
keyLabel ArrowRight = "Right"
keyLabel SpaceKey = "Space"
keyLabel EnterKey = "Return"
keyLabel TabKey = "Tab"
keyLabel BackspaceKey = "Backspace"
keyLabel LeftShiftKey = "Left Shift"
keyLabel RightShiftKey = "Right Shift"
keyLabel LeftCtrlKey = "Left Ctrl"
keyLabel RightCtrlKey = "Right Ctrl"
keyLabel (OtherKey name) = name
