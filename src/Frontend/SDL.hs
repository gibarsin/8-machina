{-# LANGUAGE OverloadedStrings #-}

module Frontend.SDL (open) where

import Control.Monad (when)
import Data.Char (ord, toLower)
import Data.IORef
import Data.Maybe (catMaybes)
import qualified SDL

import Chip8.VideoMemory (Screen, blankScreen, screenHeight, screenWidth)
import Frontend (Frontend (..), Input (..), PhysicalKey (..))
import Graphics (Colors, draw)
import Parser (Options (..))
import Sound (Speaker, closeSpeaker, openSpeaker, setSpeaker)

-- Opens a window and returns a Frontend that drives it with SDL.
open :: Options -> IO Frontend
open options = do
  SDL.initializeAll
  window <- SDL.createWindow
                   "CHIP-8"
                   SDL.defaultWindow
                   { SDL.windowInitialSize = SDL.V2 (fromIntegral initialWidth) (fromIntegral initialHeight)
                   , SDL.windowResizable = True
                   }
  SDL.showWindow window
  speaker <- openSpeaker (tone options) (volume options)
  lastScreen <- newIORef blankScreen
  return Frontend
    { pollInput = pollSDLInput window (colors options) lastScreen
    , present = \currentScreen -> do
        writeIORef lastScreen currentScreen
        draw (colors options) currentScreen window
    , setBeep = setSpeaker speaker
    , close = do
        closeSpeaker speaker
        SDL.destroyWindow window
        SDL.quit
    }
  where
    (initialWidth, initialHeight) = windowSize options

-- Polls SDL's event queue, handles a resize by itself (there is no Input
-- for it), and translates everything else into Input values.
pollSDLInput :: SDL.Window -> Colors -> IORef Screen -> IO [Input]
pollSDLInput window windowColors lastScreen = do
  events <- SDL.pollEvents
  when (any isResizeEvent events) $ do
    snapWindowSize window
    currentScreen <- readIORef lastScreen
    draw windowColors currentScreen window
  catMaybes <$> mapM toInput events

isResizeEvent :: SDL.Event -> Bool
isResizeEvent event = case SDL.eventPayload event of
  SDL.WindowSizeChangedEvent _ -> True
  _ -> False

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

-- Shrinks the window to the largest multiple of 64x32 that fits, so the
-- screen fills it without borders.
snapWindowSize :: SDL.Window -> IO ()
snapWindowSize window = do
  SDL.V2 currentWidth currentHeight <- SDL.get (SDL.windowSize window)
  let pixelSize = max 1 (min (currentWidth `div` fromIntegral screenWidth) (currentHeight `div` fromIntegral screenHeight))
      validSize = SDL.V2 (pixelSize * fromIntegral screenWidth) (pixelSize * fromIntegral screenHeight)
  when (validSize /= SDL.V2 currentWidth currentHeight) $
    SDL.windowSize window SDL.$= validSize
