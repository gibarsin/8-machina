module Register where

import Data.Array.IO
import Data.Word (Word8, Word16)
import RegisterName

type Registers = IOArray RegisterName WordRegister

type PseudoRegisters = IOArray PseudoRegisterName WordPseudoRegister

createRegisters :: IO Registers
createRegisters = newArray (V0, ST) 0

getRegisterValue :: Registers -> RegisterName -> IO WordRegister
getRegisterValue registers name = readArray registers name

setValueAtRegister :: Registers -> RegisterName -> WordRegister -> IO ()
setValueAtRegister registers name value = writeArray registers name value

addValueAtRegister :: Registers -> RegisterName -> WordRegister -> IO ()
addValueAtRegister registers name value = do
  currentVx <- getRegisterValue registers name
  setValueAtRegister registers name (currentVx + value)

createPseudoRegisters :: IO PseudoRegisters
createPseudoRegisters = newArray (I, PC) 0

getPseudoRegisterValue :: PseudoRegisters -> PseudoRegisterName -> IO WordPseudoRegister
getPseudoRegisterValue registers name = readArray registers name

setValueAtPseudoRegister :: PseudoRegisters -> PseudoRegisterName -> WordPseudoRegister -> IO ()
setValueAtPseudoRegister registers name value = writeArray registers name value
