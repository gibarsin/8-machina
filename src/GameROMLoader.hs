module GameROMLoader where

import Data.ByteString
import Memory

gameROMStartPosition :: Address
gameROMStartPosition = 0x200

loadGameROM :: Memory -> ByteString -> IO ()
loadGameROM memory gameROM = setArrayAtMemory memory gameROMStartPosition (unpack gameROM)
