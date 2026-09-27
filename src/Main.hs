{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (try)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (ord, toLower)
import Data.Maybe (catMaybes)
import qualified SDL
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString)
import System.Random (initStdGen)
import Text.Printf (printf)

import Chip8.CPU (Frame (..), errorMessage, runFrame)
import Chip8.Machine (Machine (..), newMachine, programStart)
import Chip8.Memory (memorySize)
import Chip8.VideoMemory (screenHeight, screenWidth)
import Frontend (Input (..), PhysicalKey (..))
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
  inputs <- catMaybes <$> mapM toInput events
  unless (QuitRequested `elem` inputs) $ do
    when (any isResizeEvent events) $ do
      snapWindowSize window
      draw (colors options) (screen machine) window
    let pressedKeys = updateKeypad (keyMapping options) inputs (keypad machine)
        instructionBudget = carriedInstructions + fromIntegral (speed options) / framesPerSecond
        instructionsThisFrame = floor instructionBudget
    mapM_ (\name -> hPutStrLn stderr ("Ignoring unmapped key: " ++ name))
          (unmappedKeyNames (keyMapping options) inputs)
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

isResizeEvent :: SDL.Event -> Bool
isResizeEvent event = case SDL.eventPayload event of
  SDL.WindowSizeChangedEvent _ -> True
  _ -> False

-- Temporary: duplicated in Frontend/SDL.hs (Task 2) and removed from here
-- in Task 3, once Main.hs stops handling SDL events itself.
toInput :: SDL.Event -> IO (Maybe Input)
toInput event = case SDL.eventPayload event of
  SDL.QuitEvent -> return (Just QuitRequested)
  SDL.KeyboardEvent keyEvent -> do
    key <- toPhysicalKey (SDL.keyboardEventKeysym keyEvent)
    return $ Just $ case SDL.keyboardEventKeyMotion keyEvent of
      SDL.Pressed  -> KeyDown key
      SDL.Released -> KeyUp key
  _ -> return Nothing

toPhysicalKey :: SDL.Keysym -> IO PhysicalKey
toPhysicalKey keysym = case lookup (SDL.keysymKeycode keysym) namedKeycodes of
  Just key -> return key
  Nothing -> OtherKey <$> SDL.getScancodeName (SDL.keysymScancode keysym)

-- SDL gives letter and digit keys the code of their lowercase character.
characterKeycode :: Char -> SDL.Keycode
characterKeycode character = SDL.Keycode (fromIntegral (ord (toLower character)))

namedKeycodes :: [(SDL.Keycode, PhysicalKey)]
namedKeycodes =
  [ (characterKeycode c, CharKey c) | c <- ['A' .. 'Z'] ++ ['0' .. '9'] ]
  ++ zip [SDL.KeycodeKP0, SDL.KeycodeKP1, SDL.KeycodeKP2, SDL.KeycodeKP3, SDL.KeycodeKP4
         , SDL.KeycodeKP5, SDL.KeycodeKP6, SDL.KeycodeKP7, SDL.KeycodeKP8, SDL.KeycodeKP9]
         (map KeypadKey [0 .. 9])
  ++ [ (SDL.KeycodeUp, ArrowUp), (SDL.KeycodeDown, ArrowDown), (SDL.KeycodeLeft, ArrowLeft), (SDL.KeycodeRight, ArrowRight)
     , (SDL.KeycodeSpace, SpaceKey), (SDL.KeycodeReturn, EnterKey)
     , (SDL.KeycodeTab, TabKey), (SDL.KeycodeBackspace, BackspaceKey)
     , (SDL.KeycodeLShift, LeftShiftKey), (SDL.KeycodeRShift, RightShiftKey)
     , (SDL.KeycodeLCtrl, LeftCtrlKey), (SDL.KeycodeRCtrl, RightCtrlKey)
     ]
