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
import Sound
import VideoMemory

import Control.Exception (try)
import qualified SDL as SDL
import System.Exit (die)
import System.IO.Error (ioeGetErrorString)
import Text.Printf (printf)

main :: IO ()
main = do
  options <- parse
  gameROM <- loadROMFile (romPath options)
  run options gameROM

loadROMFile :: FilePath -> IO ByteString
loadROMFile path = do
  contents <- try (Data.ByteString.readFile path)
  case contents of
    Left err -> die $ printf "Could not read ROM file %s: %s" path (ioeGetErrorString err)
    Right rom
      | romSize rom > maxROMSize ->
          die $ printf "ROM file %s is too large: %d bytes (at most %d fit in memory)" path (romSize rom) maxROMSize
      | otherwise -> return rom
  where
    romSize = Data.ByteString.length
    maxROMSize = fromIntegral (memorySize - gameROMStartPosition)

run :: Options -> ByteString -> IO ()
run options gameROM = do
  machineState <- createMachineState (quirksProfile options)
  loadFonts $ memory machineState
  loadGameROM (memory machineState) gameROM
  setPC machineState gameROMStartPosition

  -- SDL.initialize [SDL.InitVideo]
  SDL.initializeAll

  window <- SDL.createWindow
                   "CHIP-8"
                   SDL.defaultWindow
                   { SDL.windowInitialSize = SDL.V2 (fromIntegral initialWidth) (fromIntegral initialHeight)
                   , SDL.windowResizable = True
                   }
  SDL.showWindow window
  speaker <- openSpeaker (tone options) (volume options)

  frameStart <- SDL.time
  emulate options machineState window speaker frameStart 0

  closeSpeaker speaker
  SDL.destroyWindow window
  SDL.quit
  where
    (initialWidth, initialHeight) = windowSize options

framesPerSecond :: Double
framesPerSecond = 60

-- Each frame adds speed / 60 instructions to the budget and runs the whole
-- part of it; the fraction carries over, so any speed averages out exactly.
emulate :: Options -> MachineState -> SDL.Window -> Speaker -> Double -> Double -> IO ()
emulate options machineState window speaker frameStart carriedInstructions = do
  events <- SDL.pollEvents
  unless (Prelude.any isQuitEvent events) $ do
    let keyboardEvents = Prelude.filter isKeyboardEvent events
    let keyPresses = Prelude.map toKeyPress keyboardEvents
    setKeys (keyMapping options) keyPresses machineState
    when (Prelude.any isResizeEvent events) $ redraw options machineState window
    let instructionBudget = carriedInstructions + fromIntegral (speed options) / framesPerSecond
        instructionsThisFrame = floor instructionBudget
    replicateM_ instructionsThisFrame $ step options machineState window
    decTimers machineState
    soundTimer <- getRegisterValue (registers machineState) ST
    setSpeaker speaker (soundTimer > 0)
    nextFrameStart <- waitForNextFrame frameStart
    emulate options machineState window speaker nextFrameStart
            (instructionBudget - fromIntegral instructionsThisFrame)

step :: Options -> MachineState -> SDL.Window -> IO ()
step options machineState window = do
  pc <- getPC machineState
  encodedInstruction <- fetch machineState
  instruction <- maybe (die $ printf "Unknown instruction 0x%04X at address 0x%03X" encodedInstruction pc)
                       return
                       (decodeInstruction encodedInstruction)
  execute machineState instruction
  case instruction of
    (JP _) -> return ()
    (CALL _) -> return ()
    (JPV0 _) -> return ()
    CLS -> redraw options machineState window >> incPC machineState
    (DRW _ _ _) -> redraw options machineState window >> incPC machineState
    _ -> do
      incPC machineState

redraw :: Options -> MachineState -> SDL.Window -> IO ()
redraw options machineState window = do
  draw (colors options) (videoMemory machineState) window
  SDL.updateWindowSurface window

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

isResizeEvent event = case SDL.eventPayload event of
  SDL.WindowSizeChangedEvent _ -> True
  _ -> False

isKeyboardEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent _ -> True
  _ -> False

toKeyPress e =
  toKeyPress' (SDL.keyboardEventKeyMotion (getKeyboardEventData (SDL.eventPayload e))) (SDL.keyboardEventKeysym (getKeyboardEventData (SDL.eventPayload e)))

getKeyboardEventData (SDL.KeyboardEvent keyboardEventData) = keyboardEventData

toKeyPress' SDL.Pressed key  = (key, True)
toKeyPress' SDL.Released key  = (key, False)
