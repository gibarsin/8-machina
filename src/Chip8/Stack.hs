module Chip8.Stack
  ( Stack
  , emptyStack
  , push
  , pop
  ) where

import Data.Word (Word16)

-- Return addresses, most recent first. The CHIP-8 stack holds 16.
newtype Stack = Stack [Word16]

stackLimit :: Int
stackLimit = 16

emptyStack :: Stack
emptyStack = Stack []

-- Nothing when the stack is full.
push :: Word16 -> Stack -> Maybe Stack
push address (Stack addresses)
  | length addresses >= stackLimit = Nothing
  | otherwise = Just (Stack (address : addresses))

-- Nothing when the stack is empty.
pop :: Stack -> Maybe (Word16, Stack)
pop (Stack []) = Nothing
pop (Stack (address : rest)) = Just (address, Stack rest)
