module Chip8.Machine
  ( Machine (..)
  , programStart
  , newMachine
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word8)
import System.Random (StdGen)

import Chip8.Keypad (Keypad, noKeysPressed)
import Chip8.Memory (Address, Memory, emptyMemory, writeBytes)
import Chip8.Register (Registers, emptyRegisters)
import Chip8.Stack (Stack, emptyStack)
import Chip8.VideoMemory (Screen, blankScreen)
import Fonts (fonts, fontsStartPosition)
import Quirks (Quirks)

-- The whole state of a CHIP-8 machine at one moment.
data Machine = Machine
  { memory :: Memory
  , registers :: Registers
  , index :: Address          -- the I register
  , pc :: Address
  , stack :: Stack
  , delayTimer :: Word8
  , soundTimer :: Word8
  , screen :: Screen
  , keypad :: Keypad
  , quirks :: Quirks
  , generator :: StdGen       -- source of the random numbers for RND
  }

-- Programs are loaded, and start running, here.
programStart :: Address
programStart = 0x200

newMachine :: Quirks -> StdGen -> ByteString -> Machine
newMachine machineQuirks seed rom = Machine
  { memory = writeBytes programStart (ByteString.unpack rom) (writeBytes fontsStartPosition fonts emptyMemory)
  , registers = emptyRegisters
  , index = 0
  , pc = programStart
  , stack = emptyStack
  , delayTimer = 0
  , soundTimer = 0
  , screen = blankScreen
  , keypad = noKeysPressed
  , quirks = machineQuirks
  , generator = seed
  }
