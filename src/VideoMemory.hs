module VideoMemory where

import Control.Monad
import Data.Array.IO
import Data.Array.MArray
import Data.Bits
import Data.List.Split
import Memory
import qualified System.Process as SP

import Numeric (showHex, showIntAtBase)
import Fonts

type VideoMemory = IOUArray WordVideoAddress Bool
type WordVideoAddress = Integer

width = 64
height = 32

createVideoMemory :: IO VideoMemory
createVideoMemory = newArray (0, (width * height) - 1) False

clearVideoMemory :: VideoMemory -> IO ()
clearVideoMemory videoMemory = do
  (firstIndex, lastIndex) <- getBounds videoMemory
  forM_ [firstIndex .. lastIndex] $ \index -> writeArray videoMemory index False

drawSprite :: Bool -> Memory -> VideoMemory -> (WordVideoAddress, WordVideoAddress) -> Integer -> Integer -> IO Bool
drawSprite clip memory videoMemory (x, y) bytesToRead address = do
  sprite <- fmap (take (fromIntegral bytesToRead) . drop (fromIntegral address)) $ getElems memory -- TODO make an abstraction of getElems
  let boolSprite = concatMap toBoolList sprite
  drawSprite' clip videoMemory (x, y) boolSprite

toBoolList :: WordMemory -> [Bool]
toBoolList value = reverse $ toBoolList' value 0

toBoolList' :: WordMemory -> Int -> [Bool]
toBoolList' _ 8 = []
toBoolList' value bit = testBit value bit : toBoolList' value (bit + 1)

-- The sprite's position always wraps around the screen. Its pixels past the
-- edge are cut off when clipping, or wrap to the other side otherwise.
drawSprite' :: Bool -> VideoMemory -> (WordVideoAddress, WordVideoAddress) -> [Bool] -> IO Bool
drawSprite' clip videoMemory (x, y) boolSprite = do
    (_, e) <- foldM drawPixel (0, False) boolSprite
    return e
  where
    originX = x `mod` width
    originY = y `mod` height
    drawPixel (index, erased) boolBit
      | clip && (pixelX >= width || pixelY >= height) = return (index + 1, erased)
      | otherwise = do
          pixelState <- getPixelState videoMemory (pixelX, pixelY)
          let erased' = pixelState && boolBit
          when boolBit $ drawPixel' videoMemory (pixelX, pixelY) (not erased')
          return (index + 1, erased' || erased)
      where
        pixelX = originX + index `mod` 8
        pixelY = originY + index `div` 8

getPixelState :: VideoMemory -> (WordVideoAddress, WordVideoAddress) -> IO Bool
getPixelState videoMemory (x, y) = readArray videoMemory $ getVideoIndex (x, y)

getVideoIndex :: (WordVideoAddress, WordVideoAddress) -> WordVideoAddress
getVideoIndex (x, y) = (y `mod` height) * width + (x `mod` width)

drawPixel' :: VideoMemory -> (WordVideoAddress, WordVideoAddress) -> Bool -> IO ()
drawPixel' videoMemory (x, y) pixelState = writeArray videoMemory (getVideoIndex (x, y)) pixelState

printInConsole :: VideoMemory -> IO ()
printInConsole videoMemory = do
  videoList <- getElems videoMemory
  let stringVideo = map (\boolBit -> if boolBit then 'X' else ' ') videoList
  prettyVideo <- mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf (fromIntegral width) stringVideo
  print prettyVideo
  -- _ <- SP.system "clear"
  return ()
