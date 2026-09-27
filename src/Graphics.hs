module Graphics where

import Control.Monad

import  SDL
import VideoMemory
import Data.Array.MArray
import Data.Word (Word8)

-- Red, green and blue components.
type Color = (Word8, Word8, Word8)

data Colors = Colors
  { foregroundColor :: Color
  , backgroundColor :: Color
  }

-- Draws the screen at the largest whole-number pixel size that fits the
-- window, centred, with the background color filling the rest.
draw :: Colors -> VideoMemory -> SDL.Window -> IO ()
draw colors vm window = do
  surface <- SDL.getWindowSurface window
  V2 windowWidth windowHeight <- SDL.surfaceDimensions surface
  let pixelSize = max 1 (min (windowWidth `div` fromIntegral width) (windowHeight `div` fromIntegral height))
      offset = V2 ((windowWidth - pixelSize * fromIntegral width) `div` 2)
                  ((windowHeight - pixelSize * fromIntegral height) `div` 2)
  SDL.surfaceFillRect surface Nothing (color colors False)
  forM_ [0..(width - 1)] (\ x ->
    forM_ [0..(height - 1)] (\ y ->
      do
        let area = Rectangle
                     (P (offset + V2 (fromIntegral x * pixelSize) (fromIntegral y * pixelSize)))
                     (V2 pixelSize pixelSize)
        ps <- getPixelState vm (x, y)
        SDL.surfaceFillRect surface (Just area) (color colors ps)))

color :: Colors -> Bool -> V4 Word8
color colors pixelOn = V4 red green blue maxBound
  where
    (red, green, blue) = if pixelOn then foregroundColor colors else backgroundColor colors
