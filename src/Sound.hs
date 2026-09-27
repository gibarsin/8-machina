{-# LANGUAGE GADTs #-}

module Sound where

import Control.Monad
import Data.Int (Int16)
import Data.IORef
import qualified Data.Vector.Storable.Mutable as MV
import qualified SDL

type Speaker = SDL.AudioDevice

sampleRate :: Int
sampleRate = 44100

toneFrequency :: Int
toneFrequency = 440

amplitude :: Int16
amplitude = 3000

-- Opens the audio device paused; it plays a square wave while unpaused.
openSpeaker :: IO Speaker
openSpeaker = do
  phase <- newIORef 0
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate (fromIntegral sampleRate)
    , SDL.openDeviceFormat = SDL.Mandate SDL.Signed16BitLEAudio
    , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
    , SDL.openDeviceSamples = 1024
    , SDL.openDeviceCallback = squareWave phase
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing
    }
  return device

setSpeaker :: Speaker -> Bool -> IO ()
setSpeaker speaker on =
  SDL.setAudioDevicePlaybackState speaker (if on then SDL.Play else SDL.Pause)

closeSpeaker :: Speaker -> IO ()
closeSpeaker = SDL.closeAudioDevice

-- Called by SDL from its audio thread whenever it needs more samples.
-- The phase carries the wave's position over from the previous buffer.
squareWave :: IORef Int -> SDL.AudioFormat sample -> MV.IOVector sample -> IO ()
squareWave phase format buffer = case format of
  SDL.Signed16BitLEAudio -> do
    let samples = MV.length buffer
        halfPeriod = sampleRate `div` (2 * toneFrequency)
    start <- readIORef phase
    forM_ [0 .. samples - 1] $ \i -> do
      let high = ((start + i) `div` halfPeriod) `mod` 2 == 0
      MV.write buffer i (if high then amplitude else negate amplitude)
    writeIORef phase ((start + samples) `mod` (2 * halfPeriod))
  _ -> error "Unsupported audio format"
