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

-- Returns False without pushing when the stack is full.
push :: Stack -> WordStackValue -> IO Bool
push stack value =
  do
    currentStackPointer <- getStackPointerValue stack
    (_, lastAddress) <- getBounds (stackMemory stack)
    if currentStackPointer > lastAddress
      then return False
      else do
        writeArray (stackMemory stack) currentStackPointer value
        modifyIORef' (stackPointer stack) (+1)
        return True

getStackPointerValue :: Stack -> IO WordStackAddress
getStackPointerValue = readIORef . stackPointer

-- Returns Nothing when the stack is empty.
pop :: Stack -> IO (Maybe WordStackValue)
pop stack =
  do
    currentStackPointer <- getStackPointerValue stack
    if currentStackPointer == 0
      then return Nothing
      else do
        modifyIORef' (stackPointer stack) (subtract 1)
        Just <$> readArray (stackMemory stack) (currentStackPointer - 1)
