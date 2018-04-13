module Graphics where

import Control.Monad

import Foreign.C.Types
import  SDL
import VideoMemory
import Data.Array.MArray

scale = 1

-- drawVideoMemory :: SDL.Window -> VideoMemory -> IO ()
-- drawVideoMemory window vm = do
--   SDL.getWindowSurface window >>=
--     (\ surface ->
--       forM_ [0..(width - 1)] (\ x  ->
--         forM_ [0..(height - 1)] (\ y ->
--           do
--             let color = SDL.V4 maxBound maxBound maxBound maxBound
--             ps <- getPixelState vm (x, y)
--             let area = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 1 1)
--             SDL.surfaceFillRect surface (Just area) color)))

draw :: VideoMemory -> SDL.Window -> IO ()
draw vm window =
    SDL.getWindowSurface window >>=
        (\ surface ->
            forM_ [0..(width - 1)] (\ x ->
                forM_ [0..(height - 1)] (\ y ->
                  do
                    let area = Rectangle
                                 (P (V2 (fromIntegral x * scale) (fromIntegral y * scale)))
                                 (V2 scale scale)
                    ps <- getPixelState vm (x, y)
                    SDL.surfaceFillRect surface (Just area) (color ps))))

color True  = SDL.V4 maxBound maxBound maxBound maxBound
color False = SDL.V4 0 0 0 0
