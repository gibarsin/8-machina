module Chip8.CPU
  ( EmulatorError (..)
  , errorMessage
  , Frame (..)
  , execute
  , step
  , runFrame
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Word (Word16, Word8)
import System.Random (uniformR)
import Text.Printf (printf)

import Chip8.Keypad (Keypad, firstPressedKey, isKeyPressed)
import Chip8.Machine
import Chip8.Memory (Address, readByte, readBytes, writeBytes)
import Chip8.Register (getRegister, setRegister)
import Chip8.Stack (pop, push)
import Chip8.VideoMemory (clearScreen, drawSprite)
import Chip8.Fonts (fontSizeInWordMemory, fontsStartPosition)
import Chip8.Instruction
import Chip8.Interpreter
import Chip8.RegisterName

data EmulatorError
  = UnknownInstruction Word16 Address
  | StackOverflow Address
  | StackUnderflow Address

errorMessage :: EmulatorError -> String
errorMessage (UnknownInstruction encoded address) =
  printf "Unknown instruction 0x%04X at address 0x%03X" encoded address
errorMessage (StackOverflow address) =
  printf "Stack overflow: more than 16 nested calls at address 0x%03X" address
errorMessage (StackUnderflow address) =
  printf "Return with an empty stack at address 0x%03X" address

-- What the front-end needs to do after a frame.
data Frame = Frame
  { screenChanged :: Bool
  , beeping :: Bool
  }

-- Runs the given number of instructions with the keypad as given, then
-- ticks the timers once, as the CHIP-8 does 60 times a second.
runFrame :: Int -> Keypad -> Machine -> Either EmulatorError (Machine, Frame)
runFrame instructions pressedKeys machine = do
  (ranMachine, changed) <- runSteps instructions (machine { keypad = pressedKeys }, False)
  let ticked = tickTimers ranMachine
  return (ticked, Frame { screenChanged = changed, beeping = soundTimer ticked > 0 })
  where
    runSteps 0 result = Right result
    runSteps remaining (current, changed) = do
      (next, drew) <- step current
      runSteps (remaining - 1 :: Int) (next, changed || drew)

tickTimers :: Machine -> Machine
tickTimers machine = machine
  { delayTimer = countDown (delayTimer machine)
  , soundTimer = countDown (soundTimer machine)
  }
  where
    countDown timer = if timer > 0 then timer - 1 else 0

-- Fetches the instruction at PC, moves PC past it and runs it. Also
-- reports whether the instruction changed the screen.
step :: Machine -> Either EmulatorError (Machine, Bool)
step machine = case decodeInstruction encoded of
  Nothing -> Left (UnknownInstruction encoded (pc machine))
  Just instruction -> do
    next <- execute instruction machine { pc = pc machine + 2 }
    return (next, changesScreen instruction)
  where
    high = readByte (pc machine) (memory machine)
    low = readByte (pc machine + 1) (memory machine)
    encoded = fromIntegral high `shiftL` 8 + fromIntegral low

changesScreen :: Instruction -> Bool
changesScreen CLS = True
changesScreen (DRW _ _ _) = True
changesScreen _ = False

-- Runs one instruction. PC already points to the next instruction.
execute :: Instruction -> Machine -> Either EmulatorError Machine
execute instruction machine = case instruction of
  -- Calls machine code on the original hardware; emulators ignore it
  SYS _ -> Right machine
  CLS -> Right machine { screen = clearScreen (screen machine) }
  RET -> case pop (stack machine) of
    Just (address, rest) -> Right machine { pc = address, stack = rest }
    Nothing -> Left (StackUnderflow instructionAddress)
  JP address -> Right machine { pc = address }
  CALL address -> case push (pc machine) (stack machine) of
    Just pushed -> Right machine { pc = address, stack = pushed }
    Nothing -> Left (StackOverflow instructionAddress)
  SERB vx value -> Right (skipIf (register vx == value))
  SNERB vx value -> Right (skipIf (register vx /= value))
  SERR vx vy -> Right (skipIf (register vx == register vy))
  LDRB vx value -> Right (setV vx value machine)
  ADDB vx value -> Right (setV vx (register vx + value) machine)
  LDRR vx vy -> Right (setV vx (register vy) machine)
  OR vx vy -> Right (logic (.|.) vx vy)
  AND vx vy -> Right (logic (.&.) vx vy)
  XOR vx vy -> Right (logic xor vx vy)
  -- The arithmetic instructions write VF after the result, so the flag wins
  -- when VF is also the destination register.
  ADD vx vy -> Right (withFlag vx (register vx + register vy) (register vx > 255 - register vy))
  SUB vx vy -> Right (withFlag vx (register vx - register vy) (register vx >= register vy))
  SUBN vx vy -> Right (withFlag vx (register vy - register vx) (register vy >= register vx))
  SHR vx vy ->
    let source = register (shiftSource vx vy)
    in Right (withFlag vx (source `shiftR` 1) (source .&. 0x01 /= 0))
  SHL vx vy ->
    let source = register (shiftSource vx vy)
    in Right (withFlag vx (source `shiftL` 1) (source .&. 0x80 /= 0))
  SNERR vx vy -> Right (skipIf (register vx /= register vy))
  LDI address -> Right machine { index = address }
  JPV0 address ->
    let offsetRegister
          | jumpUsesV0 (interpreter machine) = V0
          | otherwise = getRegisterNameByNumber (fromIntegral (address `shiftR` 8))
    in Right machine { pc = address + fromIntegral (register offsetRegister) }
  RND vx mask ->
    let (value, nextGenerator) = uniformR (0, 255 :: Word8) (generator machine)
    in Right (setV vx (value .&. mask) machine) { generator = nextGenerator }
  DRW vx vy height ->
    let rows = readBytes (index machine) (fromIntegral height) (memory machine)
        position = (fromIntegral (register vx), fromIntegral (register vy))
        (drawn, collision) = drawSprite (clipSprites (interpreter machine)) rows position (screen machine)
    in Right (setFlag collision machine { screen = drawn })
  SKP vx -> Right (skipIf (isKeyPressed (register vx) (keypad machine)))
  SKNP vx -> Right (skipIf (not (isKeyPressed (register vx) (keypad machine))))
  LDRDT vx -> Right (setV vx (delayTimer machine) machine)
  LDK vx -> Right $ case firstPressedKey (keypad machine) of
    Just key -> setV vx key machine
    -- Step back so this instruction runs again until a key is pressed
    Nothing -> machine { pc = instructionAddress }
  LDDT vx -> Right machine { delayTimer = register vx }
  LDST vx -> Right machine { soundTimer = register vx }
  ADDI vx -> Right machine { index = index machine + fromIntegral (register vx) }
  LDF vx -> Right machine
    { index = fontsStartPosition + fromIntegral (fontSizeInWordMemory * (register vx .&. 0x0F)) }
  LDB vx -> Right machine { memory = writeBytes (index machine) (bcd (register vx)) (memory machine) }
  LDIR vx -> Right (afterLoadStore vx machine
    { memory = writeBytes (index machine) (map register [V0 .. vx]) (memory machine) })
  LDRI vx ->
    let values = readBytes (index machine) (length [V0 .. vx]) (memory machine)
        loaded = foldr (uncurry setRegister) (registers machine) (zip [V0 .. vx] values)
    in Right (afterLoadStore vx machine { registers = loaded })
  where
    register name = getRegister name (registers machine)
    -- PC was moved past this instruction before it ran.
    instructionAddress = pc machine - 2
    skipIf condition = if condition then machine { pc = pc machine + 2 } else machine
    logic operation vx vy =
      let result = setV vx (register vx `operation` register vy) machine
      in if logicResetsVF (interpreter machine) then setV VF 0 result else result
    withFlag vx result flag = setFlag flag (setV vx result machine)
    shiftSource vx vy = if shiftUsesVy (interpreter machine) then vy else vx
    -- The COSMAC VIP leaves I after the last register saved or loaded.
    afterLoadStore vx updated
      | loadStoreIncrementsI (interpreter machine) = updated { index = index updated + fromIntegral (length [V0 .. vx]) }
      | otherwise = updated

setV :: RegisterName -> Word8 -> Machine -> Machine
setV name value machine = machine { registers = setRegister name value (registers machine) }

setFlag :: Bool -> Machine -> Machine
setFlag flag = setV VF (if flag then 1 else 0)

-- Hundreds, tens and ones of a byte.
bcd :: Word8 -> [Word8]
bcd value = [value `div` 100, value `div` 10 `mod` 10, value `mod` 10]
