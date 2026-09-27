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
  gamePath <- parse
  gameROM <- loadROMFile gamePath
  run gameROM

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
  speaker <- openSpeaker

  frameStart <- SDL.time
  emulate machineState window speaker frameStart

  closeSpeaker speaker
  SDL.destroyWindow window
  SDL.quit

framesPerSecond :: Double
framesPerSecond = 60

-- The CHIP-8 has no official clock speed; about 600 instructions per
-- second runs most games at their intended pace.
instructionsPerFrame :: Int
instructionsPerFrame = 10

emulate :: MachineState -> SDL.Window -> Speaker -> Double -> IO ()
emulate machineState window speaker frameStart = do
  events <- SDL.pollEvents
  unless (Prelude.any isQuitEvent events) $ do
    let keyboardEvents = Prelude.filter isKeyboardEvent events
    let keycodes = Prelude.map toKeycode keyboardEvents
    setKeys keycodes machineState
    replicateM_ instructionsPerFrame $ step machineState window
    decTimers machineState
    soundTimer <- getRegisterValue (registers machineState) ST
    setSpeaker speaker (soundTimer > 0)
    nextFrameStart <- waitForNextFrame frameStart
    emulate machineState window speaker nextFrameStart

step :: MachineState -> SDL.Window -> IO ()
step machineState window = do
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
    CLS -> redraw >> incPC machineState
    (DRW _ _ _) -> redraw >> incPC machineState
    _ -> do
      incPC machineState
  where
    redraw = do
      draw (videoMemory machineState) window
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

isKeyboardEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent _ -> True
  _ -> False

toKeycode e =
  toKeycode' (SDL.keyboardEventKeyMotion (getKeyboardEventData (SDL.eventPayload e))) (SDL.keysymKeycode (SDL.keyboardEventKeysym (getKeyboardEventData (SDL.eventPayload e))))

getKeyboardEventData (SDL.KeyboardEvent keyboardEventData) = keyboardEventData

toKeycode' SDL.Pressed keycode  = (keycode, True)
toKeycode' SDL.Released keycode  = (keycode, False)
