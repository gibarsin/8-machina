module Chip8.VideoMemory
  ( Screen
  , screenWidth
  , screenHeight
  , blankScreen
  , clearScreen
  , pixelAt
  , drawSprite
  ) where

import Data.Bits (testBit)
import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word8)

-- 64x32 monochrome pixels, row by row.
newtype Screen = Screen (Vector.Vector Bool)

screenWidth :: Int
screenWidth = 64

screenHeight :: Int
screenHeight = 32

blankScreen :: Screen
blankScreen = Screen (Vector.replicate (screenWidth * screenHeight) False)

clearScreen :: Screen -> Screen
clearScreen _ = blankScreen

pixelAt :: (Int, Int) -> Screen -> Bool
pixelAt (x, y) (Screen pixels) = pixels Vector.! (y * screenWidth + x)

-- XORs the sprite onto the screen and reports whether any lit pixel was
-- turned off. The position always wraps around the screen; pixels past
-- the edge are cut off when clipping, or wrap to the other side otherwise.
drawSprite :: Bool -> [Word8] -> (Int, Int) -> Screen -> (Screen, Bool)
drawSprite clip rows (x, y) (Screen pixels) =
  (Screen (Vector.accum (/=) pixels [ (index, True) | index <- spriteIndices ]), collision)
  where
    originX = x `mod` screenWidth
    originY = y `mod` screenHeight
    spriteIndices =
      [ (pixelY `mod` screenHeight) * screenWidth + (pixelX `mod` screenWidth)
      | (row, rowNumber) <- zip rows [0 ..]
      , column <- [0 .. 7]
      , testBit row (7 - column)
      , let pixelX = originX + column
            pixelY = originY + rowNumber
      , not clip || (pixelX < screenWidth && pixelY < screenHeight)
      ]
    collision = any (pixels Vector.!) spriteIndices
