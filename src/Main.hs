module Main where

import Control.Monad
import CPU
import Data.ByteString
import Fonts
import GameROMLoader
import Instruction
import MachineState
import Memory
import Parser
import Register
import RegisterName
import VideoMemory

main :: IO ()
main = do
  gamePath <- parse
  gameROM <- Data.ByteString.readFile gamePath
  run gameROM

run :: ByteString -> IO ()
run gameROM = do
  machineState <- createMachineState
  loadFonts $ memory machineState
  loadGameROM (memory machineState) gameROM
  setPC machineState gameROMStartPosition
  forever $ do
    pc <- getPC machineState
    instruction <- fmap decodeInstruction $ fetch machineState
    -- _ <- Prelude.getLine -- Used for debugging instruction by instruction
    execute machineState instruction
    decTimers machineState
    case instruction of
      (JP _) -> return ()
      (CALL _) -> return ()
      (JPV0 _) -> return ()
      _ -> incPC machineState
