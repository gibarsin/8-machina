module MachineState where

import Memory
import Register
import RegisterName
import Stack
import VideoMemory

data MachineState = MachineState
  {
    memory :: Memory
  , videoMemory :: VideoMemory
  , registers :: Registers
  , pseudoRegisters :: PseudoRegisters
  , stack :: Stack
  }

memorySize :: Address
memorySize = 4096

twoByteWords = 2

createMachineState :: IO MachineState
createMachineState =
  do
    newMemory <- createMemory memorySize
    newVideoMemory <- createVideoMemory
    newRegisters <- createRegisters
    newPseudoRegisters <- createPseudoRegisters
    newStack <- createStack
    return MachineState {
        memory = newMemory
      , videoMemory = newVideoMemory
      , registers = newRegisters
      , pseudoRegisters = newPseudoRegisters
      , stack = newStack
    }

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

decTimers :: MachineState -> IO ()
decTimers machineState = do
  decTimer (registers machineState) DT
  decTimer (registers machineState) ST

decTimer :: Registers -> RegisterName -> IO ()
decTimer registers name = do
  register <- getRegisterValue registers name
  if register > 0 then
    setValueAtRegister registers name (register - 1)
  else return ()
