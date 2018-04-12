module Memory where
import Control.Monad
import Data.Array.IO
import Data.Word

type Memory = IOArray Address WordMemory

type Address = Word16

type WordMemory = Word8

createMemory :: Address -> IO Memory
createMemory size = newArray (0, size - 1) 0

getWordFromMemory :: Memory -> Address -> IO WordMemory
getWordFromMemory memory address = readArray memory address

setWordAtMemory :: Memory -> Address -> WordMemory -> IO ()
setWordAtMemory memory address value = writeArray memory address value

setArrayAtMemory :: Memory -> Address -> [WordMemory] -> IO ()
setArrayAtMemory memory startPosition array = zipWithM_ (writeArray memory) [startPosition..] array
