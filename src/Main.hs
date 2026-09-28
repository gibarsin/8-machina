module Main where

import System.Random (initStdGen)

import Chip8.Machine (newMachine)
import qualified Emulator
import qualified Frontend.Select
import Parser
import Rom (loadROMFile)

main :: IO ()
main = do
  options <- parse
  gameROM <- loadROMFile (romPath options)
  seed <- initStdGen
  let machine = newMachine (interpreterProfile options) seed gameROM
  frontend <- Frontend.Select.open options
  Emulator.run frontend (speed options) (keyMapping options) machine
