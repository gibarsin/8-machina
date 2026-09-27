module Chip8.Register
  ( Registers
  , emptyRegisters
  , getRegister
  , setRegister
  ) where

import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word8)
import Chip8.RegisterName (RegisterName)

-- The sixteen general-purpose registers, V0 to VF.
newtype Registers = Registers (Vector.Vector Word8)

emptyRegisters :: Registers
emptyRegisters = Registers (Vector.replicate 16 0)

getRegister :: RegisterName -> Registers -> Word8
getRegister name (Registers values) = values Vector.! fromEnum name

setRegister :: RegisterName -> Word8 -> Registers -> Registers
setRegister name value (Registers values) = Registers (values Vector.// [(fromEnum name, value)])
