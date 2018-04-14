{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import CPU
import Data.ByteString
import Fonts
import GameROMLoader
import Graphics
import Instruction
import MachineState
import Memory
import Parser
import Register
import RegisterName
import VideoMemory

import qualified SDL

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

  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow
                   "CHIP-8"
                   SDL.defaultWindow
                   { SDL.windowInitialSize = SDL.V2 (fromIntegral width * scale) (fromIntegral height * scale) }
  -- SDL.showWindow window

  forever $ do
    pc <- getPC machineState
    instruction <- fmap decodeInstruction $ fetch machineState
    -- print instruction
    -- _ <- Prelude.getLine -- Used for debugging instruction by instruction
    execute machineState instruction
    decTimers machineState
    -- getRegisterValue (registers machineState) V3 >>= print
    -- getRegisterValue (registers machineState) VE >>= print
    case instruction of
      (JP _) -> return ()
      (CALL _) -> return ()
      (JPV0 _) -> return ()
      (DRW _ _ _) -> do
        draw (videoMemory machineState) window
        SDL.updateWindowSurface window
        incPC machineState
      (LDB _) -> do
        i <- getI machineState
        -- getWordFromMemory (memory machineState) i >>= print
        -- getWordFromMemory (memory machineState) (i+1) >>= print
        -- getWordFromMemory (memory machineState) (i+2) >>= print
        -- print "askdjsalkdjhaskldjsadklshdlkshdsakljdhaskl"
        incPC machineState
      _ -> incPC machineState
