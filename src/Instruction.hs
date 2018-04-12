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
  | SHR   RegisterName
  | SUBN  RegisterName  RegisterName
  | SHL   RegisterName
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

decodeInstruction :: WordEncodedInstruction -> Instruction
decodeInstruction encodedInstruction = case opcode of
  0x0 -> case lowestByte of
    0xE0  -> CLS
    0xEE  -> RET
    _ -> SYS address
  0x1 -> JP   address
  0x2 -> CALL address
  0x3 -> SERB vx lowestByte
  0x4 -> SNERB vx lowestByte
  0x5 -> case lowestNibble of
    0x0 -> SERR  vx vy
    _ -> error ("Invalid encoded instruction = " ++ show encodedInstruction)
  0x6 -> LDRB vx lowestByte
  0x7 -> ADDB vx lowestByte
  0x8 -> case lowestNibble of
    0x0 -> LDRR vx vy
    0x1 -> OR   vx vy
    0x2 -> AND  vx vy
    0x3 -> XOR  vx vy
    0x4 -> ADD  vx vy
    0x5 -> SUB  vx vy
    0x6 -> SHR  vx
    0x7 -> SUBN vx vy
    0xE -> SHL  vx
    _ -> error ("Invalid encoded instruction = " ++ show encodedInstruction)
  0x9 -> case lowestNibble of
    0x0 -> SNERR vx vy
    _ -> error ("Invalid encoded instruction = " ++ show encodedInstruction)
  0xA -> LDI address
  0xB -> JPV0 address
  0xC -> RND  vx lowestByte
  0xD -> DRW  vx vy lowestNibble
  0xE -> case lowestByte of
    0x9E -> SKP   vx
    0xA1 -> SKNP  vx
    _ -> error ("Invalid encoded instruction = " ++ show encodedInstruction)
  0xF -> case lowestByte of
    0x7   -> LDRDT vx
    0xA   -> LDK  vx
    0x15  -> LDDT vx
    0x18  -> LDST vx
    0x1E  -> ADDI vx
    0x29  -> LDF  vx
    0x33  -> LDB  vx
    0x55  -> LDIR vx
    0x65  -> LDRI vx
    _ -> error ("Invalid encoded instruction = " ++ show encodedInstruction)
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
