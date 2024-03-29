module CPU where

import Control.Monad
import Control.Monad.Random
import Data.Bits
import Fonts
import Instruction
import Keyboard
import MachineState
import Memory
import Register
import RegisterName
import Stack
import VideoMemory

fetch :: MachineState -> IO WordEncodedInstruction
fetch machineState = do
  pc <- getPC machineState
  highbyte <- getWordFromMemory (memory machineState) (pc)
  lowByte <- getWordFromMemory (memory machineState) (pc + 1)
  return $ (fromIntegral highbyte `shiftL` 8) + fromIntegral lowByte

execute ::  MachineState -> Instruction -> IO ()

execute machineState (CLS) = do
  print "CLS"
  return () -- TODO

execute machineState (RET) = do
  addressPopped <- pop (stack machineState)
  setPC machineState addressPopped

execute machineState (JP address) = setPC machineState address

execute machineState (CALL address) = do
  currPC <- getPC machineState
  push (stack machineState) currPC
  setPC machineState address

execute machineState (SERB vx value) = do
  registerValue <- getRegisterValue (registers machineState) vx
  when (registerValue == value) $ incPC machineState

execute machineState (SNERB vx value) = do
  registerValue <- getRegisterValue (registers machineState) vx
  when (registerValue /= value) $ incPC machineState

execute machineState (SERR vx vy) = do
  registerX <- getRegisterValue (registers machineState) vx
  registerY <- getRegisterValue (registers machineState) vy
  when (registerX == registerY) $ incPC machineState

execute machineState (LDRB vx value) = setValueAtRegister (registers machineState) vx value

execute machineState (ADDB vx value) = addValueAtRegister (registers machineState) vx value

execute machineState (LDRR vx vy) = do
  registerY <- getRegisterValue (registers machineState) vy
  setValueAtRegister (registers machineState) vx registerY

execute machineState (OR vx vy) = operateRegisters machineState vx vx (.|.) vy

execute machineState (AND vx vy) = operateRegisters machineState vx vx (.&.) vy

execute machineState (XOR vx vy) = operateRegisters machineState vx vx xor vy

execute machineState (ADD vx vy) = do
  registerX <- getRegisterValue (registers machineState) vx
  registerY <- getRegisterValue (registers machineState) vy
  setCarry machineState $ if registerX > 255 - registerY then 1 else 0
  operateRegisters  machineState vx vx (+) vy

execute machineState (SUB vx vy) = do
  registerX <- getRegisterValue (registers machineState) vx
  registerY <- getRegisterValue (registers machineState) vy
  setCarry machineState $ if registerX > registerY then 1 else 0
  operateRegisters  machineState vx vy subtract vx

execute machineState (SHR vx vy) = do
  registerY <- getRegisterValue (registers machineState) vy
  setCarry machineState $ registerY .&. 0x1
  setValueAtRegister (registers machineState) vx (registerY `shiftR` 1)

execute machineState (SUBN vx vy) = do
  registerX <- getRegisterValue (registers machineState) vx
  registerY <- getRegisterValue (registers machineState) vy
  setCarry machineState $ if registerY > registerX then 1 else 0
  operateRegisters  machineState vx vx subtract vy

execute machineState (SHL vx vy) = do
  registerY <- getRegisterValue (registers machineState) vy
  setCarry machineState $ if (registerY .&. 0x80) == 0x80 then 1 else 0
  setValueAtRegister (registers machineState) vx (registerY `shiftL` 1)

execute machineState (SNERR vx vy) = do
  registerX <- getRegisterValue (registers machineState) vx
  registerY <- getRegisterValue (registers machineState) vy
  when (registerX /= registerY) $ incPC machineState

execute machineState (LDI address) = setI machineState address

execute machineState (JPV0 address) = do
  v0 <- getRegisterValue (registers machineState) V0
  setPC machineState (address + fromIntegral v0)

execute machineState (RND vx value) = do
  random <- getStdRandom (randomR (0, 255)) :: IO WordRegister
  setValueAtRegister (registers machineState) vx (random .&. value)

execute machineState (DRW vx vy bytesToRead) = do
  x <- getRegisterValue (registers machineState) vx
  y <- getRegisterValue (registers machineState) vy
  address <- getI machineState
  erased <- drawSprite (memory machineState) (videoMemory machineState) (fromIntegral x, fromIntegral y) (fromIntegral bytesToRead) (fromIntegral address)
  setCarry machineState $ if erased then 1 else 0

execute machineState (SKP vx) = do
  print "Skip if pressed"
  registerX <- getRegisterValue (registers machineState) vx
  pressed <- isKeyPressed (keypad machineState) registerX
  when pressed $ incPC machineState

execute machineState (SKNP vx) = do
  print "Skip if not pressed"
  registerX <- getRegisterValue (registers machineState) vx
  pressed <- isKeyPressed (keypad machineState) registerX
  print pressed
  when (not pressed) $ incPC machineState

execute machineState (LDRDT vx) = do
    dt <- getRegisterValue (registers machineState) DT
    setValueAtRegister (registers machineState) vx dt

execute machineState (LDK vx) = do
    dummyLine <- getLine -- TODO hack to halt until there is user input
    return ()

-- TODO make abstraction of LDDT and LDST
execute machineState (LDDT vx) = do
    registerX <- getRegisterValue (registers machineState) vx
    setValueAtRegister (registers machineState) DT registerX

execute machineState (LDST vx) = do
    registerX <- getRegisterValue (registers machineState) vx
    setValueAtRegister (registers machineState) ST registerX

execute machineState (ADDI vx) = do
    registerI <- getI machineState
    registerX <- getRegisterValue (registers machineState) vx
    setI machineState (registerI + (fromIntegral registerX))

execute machineState (LDF vx) = do
    registerX <- getRegisterValue (registers machineState) vx
    setI machineState (fontsStartPosition + fromIntegral (fontSizeInWordMemory * (registerX .&. 0x0f)))

execute machineState (LDB vx) = do
    registerX <- getRegisterValue (registers machineState) vx
    registerI <- getI machineState
    let (bcd0, bcd1, bcd2) =  bcd registerX
    setWordAtMemory (memory machineState) (registerI + 0) bcd0
    setWordAtMemory (memory machineState) (registerI + 1) bcd1
    setWordAtMemory (memory machineState) (registerI + 2) bcd2

execute machineState (LDIR vx) = do

    foldM_ (\a registerName -> do
      registerI <- getI machineState
      valueToStore <- getRegisterValue (registers machineState) registerName
      setWordAtMemory (memory machineState) (registerI) valueToStore
      incI machineState
      return $ a + 1
      ) 0 [V0 .. vx]

execute machineState (LDRI vx) = do
    foldM_ (\a registerName -> do
      registerI <- getI machineState
      valueToStore <- getWordFromMemory (memory machineState) (registerI)
      setValueAtRegister (registers machineState) registerName valueToStore
      incI machineState
      return $ a + 1
      ) 0 [V0 .. vx]

execute _ _ = error "Instruction decoded correctly but not implemented in CPU."

operateRegisters machineState registerToSave registerNameA op registerNameB = do
  registerA <- getRegisterValue (registers machineState) registerNameA
  registerB <- getRegisterValue (registers machineState) registerNameB
  setValueAtRegister (registers machineState) registerToSave (registerA `op` registerB)

bcd n =
  case map (read . (:[])) (show n) of
    [z]     -> (0,0,z)
    [y,z]   -> (0,y,z)
    [x,y,z] -> (x,y,z)
    _ -> error $ "Impossible pattern match for: " ++ show n
