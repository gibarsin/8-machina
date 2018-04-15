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

import qualified SDL as SDL

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

  -- SDL.initialize [SDL.InitVideo]
  SDL.initializeAll

  window <- SDL.createWindow
                   "CHIP-8"
                   SDL.defaultWindow
                   { SDL.windowInitialSize = SDL.V2 (fromIntegral width * scale) (fromIntegral height * scale) }
  SDL.showWindow window

  forever $ do
    events <- SDL.pollEvents
    let keyboardEvents = Prelude.filter isKeyboardEvent events
    let keycodes = Prelude.map toKeycode keyboardEvents
    setKeys keycodes machineState
    pc <- getPC machineState
    instruction <- fmap decodeInstruction $ fetch machineState
    execute machineState instruction
    decTimers machineState
    case instruction of
      (JP _) -> return ()
      (CALL _) -> return ()
      (JPV0 _) -> return ()
      (DRW _ _ _) -> do
        draw (videoMemory machineState) window
        SDL.updateWindowSurface window
        incPC machineState
      _ -> do
        incPC machineState

isKeyboardEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent _ -> True
  _ -> False

toKeycode e =
  toKeycode' (SDL.keyboardEventKeyMotion (getKeyboardEventData (SDL.eventPayload e))) (SDL.keysymKeycode (SDL.keyboardEventKeysym (getKeyboardEventData (SDL.eventPayload e))))

getKeyboardEventData (SDL.KeyboardEvent keyboardEventData) = keyboardEventData

toKeycode' SDL.Pressed keycode  = (keycode, True)
toKeycode' SDL.Released keycode  = (keycode, False)
