module Instruction where

import Control.Monad
import Data.Word (Word8, Word16)
import Data.Bits

import Memory
import Register
import RegisterName

type WordEncodedInstruction = Word16

type WordOpCode = Word8

data Instruction =
    SYS Address
  | CLS
  | RET
  | JP    Address
  | CALL  Address
  | SERB  RegisterName  WordRegister
  | SNERB RegisterName  WordRegister
  | SERR  RegisterName  RegisterName
  | LDRB  RegisterName  WordRegister
  | ADDB  RegisterName  WordRegister
  | LDRR  RegisterName  RegisterName
  | OR    RegisterName  RegisterName
  | AND   RegisterName  RegisterName
  | XOR   RegisterName  RegisterName
  | ADD   RegisterName  RegisterName
  | SUB   RegisterName  RegisterName
  | SHR   RegisterName  RegisterName
  | SUBN  RegisterName  RegisterName
  | SHL   RegisterName  RegisterName
  | SNERR RegisterName  RegisterName
  | LDI   Address
  | JPV0  Address
  | RND   RegisterName  WordRegister
  | DRW   RegisterName  RegisterName  WordRegister
  | SKP   RegisterName
  | SKNP  RegisterName
  | LDRDT RegisterName
  | LDK   RegisterName
  | LDDT  RegisterName
  | LDST  RegisterName
  | ADDI  RegisterName
  | LDF   RegisterName
  | LDB   RegisterName
  | LDIR  RegisterName
  | LDRI  RegisterName
  deriving (Show)

-- Returns Nothing for bit patterns that are not CHIP-8 instructions.
decodeInstruction :: WordEncodedInstruction -> Maybe Instruction
decodeInstruction encodedInstruction = case opcode of
  0x0 -> case lowestByte of
    0xE0  -> Just CLS
    0xEE  -> Just RET
    _ -> Just (SYS address)
  0x1 -> Just (JP   address)
  0x2 -> Just (CALL address)
  0x3 -> Just (SERB vx lowestByte)
  0x4 -> Just (SNERB vx lowestByte)
  0x5 -> case lowestNibble of
    0x0 -> Just (SERR  vx vy)
    _ -> Nothing
  0x6 -> Just (LDRB vx lowestByte)
  0x7 -> Just (ADDB vx lowestByte)
  0x8 -> case lowestNibble of
    0x0 -> Just (LDRR vx vy)
    0x1 -> Just (OR   vx vy)
    0x2 -> Just (AND  vx vy)
    0x3 -> Just (XOR  vx vy)
    0x4 -> Just (ADD  vx vy)
    0x5 -> Just (SUB  vx vy)
    0x6 -> Just (SHR  vx vy)
    0x7 -> Just (SUBN vx vy)
    0xE -> Just (SHL  vx vy)
    _ -> Nothing
  0x9 -> case lowestNibble of
    0x0 -> Just (SNERR vx vy)
    _ -> Nothing
  0xA -> Just (LDI address)
  0xB -> Just (JPV0 address)
  0xC -> Just (RND  vx lowestByte)
  0xD -> Just (DRW  vx vy lowestNibble)
  0xE -> case lowestByte of
    0x9E -> Just (SKP   vx)
    0xA1 -> Just (SKNP  vx)
    _ -> Nothing
  0xF -> case lowestByte of
    0x7   -> Just (LDRDT vx)
    0xA   -> Just (LDK  vx)
    0x15  -> Just (LDDT vx)
    0x18  -> Just (LDST vx)
    0x1E  -> Just (ADDI vx)
    0x29  -> Just (LDF  vx)
    0x33  -> Just (LDB  vx)
    0x55  -> Just (LDIR vx)
    0x65  -> Just (LDRI vx)
    _ -> Nothing
  _ -> Nothing
  where
    address = getAddress encodedInstruction
    lowestByte = getLowestByte encodedInstruction
    lowestNibble = getLowestNibble encodedInstruction
    opcode = getOpcode encodedInstruction
    vx = getRegisterX encodedInstruction
    vy = getRegisterY encodedInstruction

getAddress :: WordEncodedInstruction -> Address
getAddress encodedInstruction = encodedInstruction .&. 0x0FFF

getOpcode :: WordEncodedInstruction -> WordOpCode
getOpcode encodedInstruction = nibble 3 encodedInstruction

getLowestByte :: WordEncodedInstruction -> Word8
getLowestByte encodedInstruction = fromIntegral $ 0xFF .&. encodedInstruction

getLowestNibble :: WordEncodedInstruction -> Word8
getLowestNibble encodedInstruction = nibble 0 encodedInstruction

getRegisterX :: WordEncodedInstruction -> RegisterName
getRegisterX encodedInstruction = getRegisterNameByNumber $ nibble 2 encodedInstruction

getRegisterY :: WordEncodedInstruction -> RegisterName
getRegisterY encodedInstruction = getRegisterNameByNumber $ nibble 1 encodedInstruction

nibble :: Num a => Int -> Word16 -> a
nibble n w = fromIntegral $ w `shiftR` (n * 4) .&. 0xF
