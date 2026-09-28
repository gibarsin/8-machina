module Frontend.Brick (open) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, put)
import Data.Char (isDigit, isLower, isUpper, toUpper)
import Data.IORef
import qualified Data.Map as Map
import GHC.Clock (getMonotonicTime)
import System.IO (hFlush, stdout)

import qualified Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (raw)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

import Chip8.VideoMemory (Screen, blankScreen, pixelAt, screenHeight, screenWidth)
import Frontend (Color, Colors (..), Frontend (..), Input (..), PhysicalKey (..))
import Parser (Options (..))

-- How long a key can go without a repeat before it counts as released.
-- Held keys in a real terminal repeat roughly every 30ms once they start,
-- so 100ms is a few missed repeats, not a hair trigger.
releaseTimeout :: Double
releaseTimeout = 0.1

-- How often the ticker checks for released keys and nudges a redraw.
tickInterval :: Int
tickInterval = 16000

-- Events pushed onto the channel brick's own loop reads, from outside it.
data UiEvent = ScreenUpdated Screen | BeepChanged Bool | Tick | CloseRequested

data UiState = UiState
  { currentScreen :: Screen
  , currentColors :: Colors
  , wasBeeping :: Bool
  , heldSince :: Map.Map PhysicalKey Double  -- when each held key last repeated
  }

-- Opens a Frontend that draws in the current terminal with brick. Esc
-- quits; there is no window, so --size does not apply, and there is no
-- audio device, so --tone is ignored and the sound timer rings the
-- terminal bell instead (silenced by --volume 0, same as no sound at all).
open :: Options -> IO Frontend
open options = do
  pending <- newIORef []
  uiChan <- newBChan 20
  done <- newEmptyMVar
  let buildVty = mkVty V.defaultConfig
      muteBell = volume options == 0
      initialState = UiState
        { currentScreen = blankScreen
        , currentColors = colors options
        , wasBeeping = False
        , heldSince = Map.empty
        }
  vty <- buildVty
  void $ forkIO $ forever $ threadDelay tickInterval >> writeBChan uiChan Tick
  void $ forkIO $ do
    _ <- Brick.customMain vty buildVty (Just uiChan) (brickApp pending muteBell) initialState
    putMVar done ()
  return Frontend
    { pollInput = atomicModifyIORef' pending (\inputs -> ([], reverse inputs))
    , present = writeBChan uiChan . ScreenUpdated
    , setBeep = writeBChan uiChan . BeepChanged
    , close = writeBChan uiChan CloseRequested >> takeMVar done
    }

pushInput :: IORef [Input] -> Input -> IO ()
pushInput pending input = atomicModifyIORef' pending (\inputs -> (input : inputs, ()))

brickApp :: IORef [Input] -> Bool -> Brick.App UiState UiEvent ()
brickApp pending muteBell = Brick.App
  { Brick.appDraw = \state -> [center (raw (screenImage (currentColors state) (currentScreen state)))]
  , Brick.appChooseCursor = Brick.neverShowCursor
  , Brick.appHandleEvent = handleEvent pending muteBell
  , Brick.appStartEvent = return ()
  , Brick.appAttrMap = const (Brick.attrMap V.defAttr [])
  }

handleEvent :: IORef [Input] -> Bool -> Brick.BrickEvent () UiEvent -> Brick.EventM () UiState ()
handleEvent pending muteBell event = case event of
  Brick.AppEvent (ScreenUpdated screen) -> modify (\s -> s { currentScreen = screen })
  Brick.AppEvent (BeepChanged beeping) -> do
    state <- get
    when (beeping && not (wasBeeping state) && not muteBell) $
      liftIO (putStr "\a" >> hFlush stdout)
    put state { wasBeeping = beeping }
  Brick.AppEvent Tick -> do
    now <- liftIO getMonotonicTime
    state <- get
    let (stillHeld, released) = Map.partition (\lastSeen -> now - lastSeen < releaseTimeout) (heldSince state)
    liftIO $ forM_ (Map.keys released) $ \key -> pushInput pending (KeyUp key)
    put state { heldSince = stillHeld }
  Brick.AppEvent CloseRequested -> Brick.halt
  Brick.VtyEvent (V.EvKey V.KEsc _) -> do
    liftIO $ pushInput pending QuitRequested
    Brick.halt
  Brick.VtyEvent (V.EvKey key _) -> do
    now <- liftIO getMonotonicTime
    state <- get
    let physicalKey = toPhysicalKey key
    when (Map.notMember physicalKey (heldSince state)) $
      liftIO $ pushInput pending (KeyDown physicalKey)
    put state { heldSince = Map.insert physicalKey now (heldSince state) }
  _ -> return ()

-- vty has no separate key for Left/Right Shift or Ctrl (terminals do not
-- report them on their own), and no reliable numeric keypad detection, so
-- KeypadKey, LeftShiftKey, RightShiftKey, LeftCtrlKey and RightCtrlKey are
-- never produced by this front-end.
toPhysicalKey :: V.Key -> PhysicalKey
toPhysicalKey (V.KChar ' ')  = SpaceKey
toPhysicalKey (V.KChar '\t') = TabKey
toPhysicalKey (V.KChar c)
  | isUpper c || isLower c = CharKey (toUpper c)
  | isDigit c = CharKey c
  | otherwise = OtherKey [c]
toPhysicalKey V.KEnter = EnterKey
toPhysicalKey V.KBS = BackspaceKey
toPhysicalKey V.KUp = ArrowUp
toPhysicalKey V.KDown = ArrowDown
toPhysicalKey V.KLeft = ArrowLeft
toPhysicalKey V.KRight = ArrowRight
toPhysicalKey key = OtherKey (show key)

-- Two CHIP-8 pixel rows per terminal character, using half-block glyphs,
-- since a terminal character is roughly twice as tall as it is wide.
screenImage :: Colors -> Screen -> V.Image
screenImage screenColors currentScreen =
  V.vertCat [ rowImage y | y <- [0, 2 .. screenHeight - 1] ]
  where
    attr = V.defAttr
             `V.withForeColor` toVtyColor (foregroundColor screenColors)
             `V.withBackColor` toVtyColor (backgroundColor screenColors)
    rowImage y = V.horizCat
      [ V.char attr (blockChar (pixelAt (x, y) currentScreen) (pixelAt (x, y + 1) currentScreen))
      | x <- [0 .. screenWidth - 1] ]
    blockChar True  True  = '█'
    blockChar True  False = '▀'
    blockChar False True  = '▄'
    blockChar False False = ' '

toVtyColor :: Color -> V.Color
toVtyColor (red, green, blue) = V.rgbColor red green blue
