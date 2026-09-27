module Emulator (run) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import GHC.Clock (getMonotonicTime)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

import Chip8.CPU (TimerTick (..), errorMessage, runTick)
import Chip8.Machine (Machine (..))
import Frontend (Frontend (..), Input (..))
import Keyboard (KeyMapping, unmappedKeyNames, updateKeypad)

framesPerSecond :: Double
framesPerSecond = 60

-- Runs frames through the given front-end until it reports a quit or the
-- machine hits an error, then closes the front-end. Each frame adds
-- speed / 60 instructions to a running budget and executes the whole part
-- of it; the fraction carries over, so any speed averages out exactly.
run :: Frontend -> Int -> KeyMapping -> Machine -> IO ()
run frontend speed keyMapping machine = do
  frameStart <- getMonotonicTime
  loop machine frameStart 0
  where
    loop machine frameStart carriedInstructions = do
      inputs <- pollInput frontend
      if QuitRequested `elem` inputs
        then close frontend
        else do
          mapM_ (\name -> hPutStrLn stderr ("Ignoring unmapped key: " ++ name))
                (unmappedKeyNames keyMapping inputs)
          let pressedKeys = updateKeypad keyMapping inputs (keypad machine)
              instructionBudget = carriedInstructions + fromIntegral speed / framesPerSecond
              instructionsThisFrame = floor instructionBudget
          case runTick instructionsThisFrame pressedKeys machine of
            Left emulatorError -> close frontend >> die (errorMessage emulatorError)
            Right (nextMachine, tick) -> do
              when (screenChanged tick) $ present frontend (screen nextMachine)
              setBeep frontend (beeping tick)
              nextFrameStart <- waitUntil (frameStart + 1 / framesPerSecond)
              loop nextMachine nextFrameStart
                   (instructionBudget - fromIntegral instructionsThisFrame)

-- Sleeps until the given time and reports when the next frame actually
-- starts. If emulation fell behind, the next frame starts now instead of
-- trying to catch up.
waitUntil :: Double -> IO Double
waitUntil frameEnd = do
  currentTime <- getMonotonicTime
  when (currentTime < frameEnd) $ threadDelay (round ((frameEnd - currentTime) * 1000000))
  return (max frameEnd currentTime)
