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

-- clearVideoMemory :: VideoMemory -> IO ()
-- clearVideoMemory videoMemory = fill videoMemory False

drawSprite :: Memory -> VideoMemory -> (WordVideoAddress, WordVideoAddress) -> Integer -> Integer -> IO Bool
drawSprite memory videoMemory (x, y) bytesToRead address = do
  mem <- getElems memory
  -- print (address >= fromIntegral fontsStartPosition && address <= fromIntegral fontsStartPosition + 5 * 16)
  -- print mem
  print address
  print bytesToRead
  sprite <- fmap (take (fromIntegral bytesToRead) . drop (fromIntegral address)) $ getElems memory -- TODO make an abstraction of getElems
  print sprite
  let boolSprite = concatMap toBoolList sprite
  drawSprite' videoMemory (x, y) boolSprite

toBoolList :: WordMemory -> [Bool]
toBoolList value = reverse $ toBoolList' value 0

toBoolList' :: WordMemory -> Int -> [Bool]
toBoolList' _ 8 = []
toBoolList' value bit = testBit value bit : toBoolList' value (bit + 1)

drawSprite' :: VideoMemory -> (WordVideoAddress, WordVideoAddress) -> [Bool] -> IO Bool
drawSprite' videoMemory (x, y) boolSprite = do
    (_, e) <- foldM drawPixel (0, False) boolSprite
    return e
  where
    drawPixel (index, erased) boolBit = do
      let dx = index `mod` 8
      let dy = index `div` 8
      pixelState <- getPixelState videoMemory (x + dx, y + dy)
      drawPixel' videoMemory (x + dx, y + dy) (pixelState `xor` boolBit)
      return (index + 1, (pixelState && boolBit) || erased)

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
  _ <- SP.system "clear"
  return ()
