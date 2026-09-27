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

  frameStart <- SDL.time
  emulate machineState window frameStart

  SDL.destroyWindow window
  SDL.quit

framesPerSecond :: Double
framesPerSecond = 60

-- The CHIP-8 has no official clock speed; about 600 instructions per
-- second runs most games at their intended pace.
instructionsPerFrame :: Int
instructionsPerFrame = 10

emulate :: MachineState -> SDL.Window -> Double -> IO ()
emulate machineState window frameStart = do
  events <- SDL.pollEvents
  unless (Prelude.any isQuitEvent events) $ do
    let keyboardEvents = Prelude.filter isKeyboardEvent events
    let keycodes = Prelude.map toKeycode keyboardEvents
    setKeys keycodes machineState
    replicateM_ instructionsPerFrame $ step machineState window
    nextFrameStart <- waitForNextFrame frameStart
    emulate machineState window nextFrameStart

step :: MachineState -> SDL.Window -> IO ()
step machineState window = do
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

-- Sleeps until the current frame ends and returns when the next one starts.
-- If emulation fell behind, the next frame starts now instead of trying to
-- catch up.
waitForNextFrame :: Double -> IO Double
waitForNextFrame frameStart = do
  now <- SDL.time
  let frameEnd = frameStart + 1 / framesPerSecond
  when (now < frameEnd) $ SDL.delay (floor ((frameEnd - now) * 1000))
  return (max frameEnd now)

isQuitEvent event = case SDL.eventPayload event of
  SDL.QuitEvent -> True
  _ -> False

isKeyboardEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent _ -> True
  _ -> False

toKeycode e =
  toKeycode' (SDL.keyboardEventKeyMotion (getKeyboardEventData (SDL.eventPayload e))) (SDL.keysymKeycode (SDL.keyboardEventKeysym (getKeyboardEventData (SDL.eventPayload e))))

getKeyboardEventData (SDL.KeyboardEvent keyboardEventData) = keyboardEventData

toKeycode' SDL.Pressed keycode  = (keycode, True)
toKeycode' SDL.Released keycode  = (keycode, False)
