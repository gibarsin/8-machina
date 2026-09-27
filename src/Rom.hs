module Rom (loadROMFile) where

import Control.Exception (try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import System.Exit (die)
import System.IO.Error (ioeGetErrorString)
import Text.Printf (printf)

import Chip8.Machine (programStart)
import Chip8.Memory (memorySize)

-- Reads a ROM file, checking it exists and fits in the memory available
-- to programs (everything after the fonts and below programStart).
loadROMFile :: FilePath -> IO ByteString
loadROMFile path = do
  contents <- try (ByteString.readFile path)
  case contents of
    Left err -> die $ printf "Could not read ROM file %s: %s" path (ioeGetErrorString err)
    Right rom
      | ByteString.length rom > maxROMSize ->
          die $ printf "ROM file %s is too large: %d bytes (at most %d fit in memory)" path (ByteString.length rom) maxROMSize
      | otherwise -> return rom
  where
    maxROMSize = memorySize - fromIntegral programStart
