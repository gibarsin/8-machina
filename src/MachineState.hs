module MachineState where

import Control.Monad
import Keyboard
import Memory
import Register
import RegisterName
import Stack
import VideoMemory

data MachineState = MachineState
  {
    keypad :: Keypad
  , memory :: Memory
  , pseudoRegisters :: PseudoRegisters
  , registers :: Registers
  , stack :: Stack
  , videoMemory :: VideoMemory
  }

memorySize :: Address
memorySize = 4096

twoByteWords = 2

createMachineState :: IO MachineState
createMachineState = do
  newKeypad <- createKeypad
  newMemory <- createMemory memorySize
  newVideoMemory <- createVideoMemory
  newRegisters <- createRegisters
  newPseudoRegisters <- createPseudoRegisters
  newStack <- createStack
  return MachineState {
      memory = newMemory
    , keypad = newKeypad
    , pseudoRegisters = newPseudoRegisters
    , registers = newRegisters
    , stack = newStack
    , videoMemory = newVideoMemory
  }

setKeys :: [(Key, Bool)] -> MachineState -> IO ()
setKeys keysPressed machineState =
  mergeKeypad (keypad machineState) keysPressed

getPC :: MachineState -> IO Address
getPC machineState = getPseudoRegisterValue (pseudoRegisters machineState) PC

setPC :: MachineState -> Address -> IO ()
setPC machineState value =
  setValueAtPseudoRegister (pseudoRegisters machineState) PC value

incPC :: MachineState -> IO ()
incPC machineState = do
    currPC <- getPC machineState
    setValueAtPseudoRegister (pseudoRegisters machineState) PC (currPC + twoByteWords)

setCarry :: MachineState -> WordMemory -> IO ()
setCarry machineState value = setValueAtRegister (registers machineState) VF value

setI :: MachineState -> Address -> IO ()
setI machineState value = setValueAtPseudoRegister (pseudoRegisters machineState) I value

getI :: MachineState -> IO Address
getI machineState = getPseudoRegisterValue (pseudoRegisters machineState) I

incI :: MachineState -> IO ()
incI machineState = do
  currI <- getI machineState
  setValueAtPseudoRegister (pseudoRegisters machineState) I (currI + 1)

decTimers :: MachineState -> IO ()
decTimers machineState = do
  decTimer (registers machineState) DT
  decTimer (registers machineState) ST

decTimer :: Registers -> RegisterName -> IO ()
decTimer registers name = do
  register <- getRegisterValue registers name
  when (register > 0) $ setValueAtRegister registers name (register - 1)
