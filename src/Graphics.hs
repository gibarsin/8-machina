module Graphics where

import Control.Monad

import  SDL
import Data.Word (Word8)
import Chip8.VideoMemory (Screen, pixelAt, screenHeight, screenWidth)

-- Red, green and blue components.
type Color = (Word8, Word8, Word8)

data Colors = Colors
  { foregroundColor :: Color
  , backgroundColor :: Color
  }

-- Draws the screen at the largest whole-number pixel size that fits the
-- window, centred, with the background color filling the rest.
draw :: Colors -> Screen -> SDL.Window -> IO ()
draw colors currentScreen window = do
  surface <- SDL.getWindowSurface window
  V2 windowWidth windowHeight <- SDL.surfaceDimensions surface
  let pixelSize = max 1 (min (windowWidth `div` fromIntegral screenWidth) (windowHeight `div` fromIntegral screenHeight))
      offset = V2 ((windowWidth - pixelSize * fromIntegral screenWidth) `div` 2)
                  ((windowHeight - pixelSize * fromIntegral screenHeight) `div` 2)
  SDL.surfaceFillRect surface Nothing (color colors False)
  forM_ [0 .. screenWidth - 1] $ \x ->
    forM_ [0 .. screenHeight - 1] $ \y ->
      when (pixelAt (x, y) currentScreen) $ do
        let area = Rectangle
                     (P (offset + V2 (fromIntegral x * pixelSize) (fromIntegral y * pixelSize)))
                     (V2 pixelSize pixelSize)
        SDL.surfaceFillRect surface (Just area) (color colors True)
  SDL.updateWindowSurface window

color :: Colors -> Bool -> V4 Word8
color colors pixelOn = V4 red green blue maxBound
  where
    (red, green, blue) = if pixelOn then foregroundColor colors else backgroundColor colors
