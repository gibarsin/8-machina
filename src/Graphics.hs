module Graphics where

import Control.Monad

import  SDL
import VideoMemory
import Data.Array.MArray

-- Draws the screen at the largest whole-number pixel size that fits the
-- window, centred, with the background color filling the rest.
draw :: VideoMemory -> SDL.Window -> IO ()
draw vm window = do
  surface <- SDL.getWindowSurface window
  V2 windowWidth windowHeight <- SDL.surfaceDimensions surface
  let pixelSize = max 1 (min (windowWidth `div` fromIntegral width) (windowHeight `div` fromIntegral height))
      offset = V2 ((windowWidth - pixelSize * fromIntegral width) `div` 2)
                  ((windowHeight - pixelSize * fromIntegral height) `div` 2)
  SDL.surfaceFillRect surface Nothing (color False)
  forM_ [0..(width - 1)] (\ x ->
    forM_ [0..(height - 1)] (\ y ->
      do
        let area = Rectangle
                     (P (offset + V2 (fromIntegral x * pixelSize) (fromIntegral y * pixelSize)))
                     (V2 pixelSize pixelSize)
        ps <- getPixelState vm (x, y)
        SDL.surfaceFillRect surface (Just area) (color ps)))

color True  = SDL.V4 maxBound maxBound maxBound maxBound
color False = SDL.V4 minBound minBound minBound minBound
