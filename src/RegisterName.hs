module RegisterName where

import Data.Array
import Data.Word (Word8, Word16)

type WordRegister = Word8

data RegisterName =
    V0 | V1 | V2 | V3
  | V4 | V5 | V6 | V7
  | V8 | V9 | VA | VB
  | VC | VD | VE | VF
  | DT | ST
  deriving (Enum, Eq, Ix, Ord, Read, Show)

getRegisterNameByNumber :: Word8 -> RegisterName
getRegisterNameByNumber 0x0 = V0
getRegisterNameByNumber 0x1 = V1
getRegisterNameByNumber 0x2 = V2
getRegisterNameByNumber 0x3 = V3
getRegisterNameByNumber 0x4 = V4
getRegisterNameByNumber 0x5 = V5
getRegisterNameByNumber 0x6 = V6
getRegisterNameByNumber 0x7 = V7
getRegisterNameByNumber 0x8 = V8
getRegisterNameByNumber 0x9 = V9
getRegisterNameByNumber 0xA = VA
getRegisterNameByNumber 0xB = VB
getRegisterNameByNumber 0xC = VC
getRegisterNameByNumber 0xD = VD
getRegisterNameByNumber 0xE = VE
getRegisterNameByNumber 0xF = VF
getRegisterNameByNumber _ = error "Cannot obtain register name by number."

type WordPseudoRegister = Word16

data PseudoRegisterName =
    I | PC
    deriving (Enum, Eq, Ix, Ord, Read, Show)
