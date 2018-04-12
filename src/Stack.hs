module Stack where
import Data.Array.IO
import Data.IORef
import Data.Word

data Stack = Stack {
    stackMemory :: IOArray WordStackAddress WordStackValue
  , stackPointer :: IORef WordStackAddress
  }

type WordStackAddress = Word8

type WordStackValue = Word16

createStack :: IO Stack
createStack =
  do
    newMemory <- newArray (0x00, 0x0F) 0
    newStackPointer <- newIORef 0x00
    return Stack {
      stackMemory = newMemory
    , stackPointer = newStackPointer
    }

push :: Stack -> WordStackValue -> IO ()
push stack value =
  do
    currentStackPointer <- getStackPointerValue stack
    writeArray (stackMemory stack) currentStackPointer value
    modifyIORef' (stackPointer stack) (+1)

getStackPointerValue :: Stack -> IO WordStackAddress
getStackPointerValue = readIORef . stackPointer

pop :: Stack -> IO WordStackValue
pop stack =
  do
    modifyIORef' (stackPointer stack) (subtract 1)
    currentStackPointer <- getStackPointerValue stack
    readArray (stackMemory stack) currentStackPointer
