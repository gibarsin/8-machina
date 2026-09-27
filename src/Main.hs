{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (try)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified SDL
import System.Exit (die)
import System.IO.Error (ioeGetErrorString)
import System.Random (initStdGen)
import Text.Printf (printf)

import Chip8.CPU (Frame (..), errorMessage, runFrame)
import Chip8.Machine (Machine (..), newMachine, programStart)
import Chip8.Memory (memorySize)
import Chip8.VideoMemory (screenHeight, screenWidth)
import Graphics
import Keyboard
import Parser
import Sound

main :: IO ()
main = do
  options <- parse
  gameROM <- loadROMFile (romPath options)
  seed <- initStdGen
  run options (newMachine (interpreterProfile options) seed gameROM)

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

run :: Options -> Machine -> IO ()
run options machine = do
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
  emulate options window speaker frameStart 0 machine

  closeSpeaker speaker
  SDL.destroyWindow window
  SDL.quit
  where
    (initialWidth, initialHeight) = windowSize options

framesPerSecond :: Double
framesPerSecond = 60

-- Each frame adds speed / 60 instructions to the budget and runs the whole
-- part of it; the fraction carries over, so any speed averages out exactly.
emulate :: Options -> SDL.Window -> Speaker -> Double -> Double -> Machine -> IO ()
emulate options window speaker frameStart carriedInstructions machine = do
  events <- SDL.pollEvents
  unless (any isQuitEvent events) $ do
    when (any isResizeEvent events) $ do
      snapWindowSize window
      draw (colors options) (screen machine) window
    let keyPresses = map toKeyPress (filter isKeyboardEvent events)
        pressedKeys = updateKeypad (keyMapping options) keyPresses (keypad machine)
        instructionBudget = carriedInstructions + fromIntegral (speed options) / framesPerSecond
        instructionsThisFrame = floor instructionBudget
    logUnmappedKeys (keyMapping options) keyPresses
    case runFrame instructionsThisFrame pressedKeys machine of
      Left emulatorError -> die (errorMessage emulatorError)
      Right (nextMachine, frame) -> do
        when (screenChanged frame) $ draw (colors options) (screen nextMachine) window
        setSpeaker speaker (beeping frame)
        nextFrameStart <- waitForNextFrame frameStart
        emulate options window speaker nextFrameStart
                (instructionBudget - fromIntegral instructionsThisFrame) nextMachine

-- Shrinks the window to the largest multiple of 64x32 that fits, so the
-- screen fills it without borders.
snapWindowSize :: SDL.Window -> IO ()
snapWindowSize window = do
  SDL.V2 currentWidth currentHeight <- SDL.get (SDL.windowSize window)
  let pixelSize = max 1 (min (currentWidth `div` fromIntegral screenWidth) (currentHeight `div` fromIntegral screenHeight))
      validSize = SDL.V2 (pixelSize * fromIntegral screenWidth) (pixelSize * fromIntegral screenHeight)
  when (validSize /= SDL.V2 currentWidth currentHeight) $
    SDL.windowSize window SDL.$= validSize

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
