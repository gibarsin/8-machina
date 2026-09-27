module Main where

import Control.Exception (try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import System.Exit (die)
import System.IO.Error (ioeGetErrorString)
import System.Random (initStdGen)
import Text.Printf (printf)

import Chip8.Machine (newMachine, programStart)
import Chip8.Memory (memorySize)
import qualified Emulator
import qualified Frontend.SDL as SDLFrontend
import Parser

main :: IO ()
main = do
  options <- parse
  gameROM <- loadROMFile (romPath options)
  seed <- initStdGen
  let machine = newMachine (interpreterProfile options) seed gameROM
  frontend <- SDLFrontend.open options
  Emulator.run frontend (speed options) (keyMapping options) machine

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
