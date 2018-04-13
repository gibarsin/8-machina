module Graphics where

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import VideoMemory

data Tick = Tick

type Name = ()

app :: App VideoMemory Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = undefined
          , appStartEvent = return
          , appAttrMap = const theMap
          }

drawUI :: VideoMemory -> [Widget Name]
drawUI videoMemory = [ C.center $ drawGrid videoMemory ]

drawGrid :: VideoMemory -> Widget Name
drawGrid video = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "CHIP-8")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow y | y <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord video (x, y) | x <- [0..width-1]]

drawCoord video (x, y) = do
  ps <- getPixelState video (x, y)
  drawCell ps

drawCell :: Bool -> Widget Name
drawCell True = withAttr onAttr cw
drawCell False = withAttr offAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (onAttr, V.white `on` V.white)
  , (offAttr, V.black `on` V.black)
  ]

onAttr, offAttr :: AttrName
onAttr = "onAttr"
offAttr = "offAttr"

-- handleEvent g _ = continue g
