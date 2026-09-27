{-# LANGUAGE GADTs #-}

module Sound where

import Control.Monad
import Data.Int (Int16)
import Data.IORef
import qualified Data.Vector.Storable.Mutable as MV
import qualified SDL

-- Nothing when the volume is 0, so no audio device is opened.
type Speaker = Maybe SDL.AudioDevice

sampleRate :: Int
sampleRate = 44100

-- Opens the audio device paused; it plays a square wave of the given
-- frequency (Hz) and volume (0 to 100) while unpaused.
openSpeaker :: Int -> Int -> IO Speaker
openSpeaker _ 0 = return Nothing
openSpeaker toneFrequency volume = do
  phase <- newIORef 0
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate (fromIntegral sampleRate)
    , SDL.openDeviceFormat = SDL.Mandate SDL.Signed16BitLEAudio
    , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
    , SDL.openDeviceSamples = 1024
    , SDL.openDeviceCallback = squareWave phase halfPeriod amplitude
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing
    }
  return (Just device)
  where
    halfPeriod = max 1 (sampleRate `div` (2 * toneFrequency))
    amplitude = fromIntegral (volume * fromIntegral (maxBound :: Int16) `div` 100)

setSpeaker :: Speaker -> Bool -> IO ()
setSpeaker Nothing _ = return ()
setSpeaker (Just device) on =
  SDL.setAudioDevicePlaybackState device (if on then SDL.Play else SDL.Pause)

closeSpeaker :: Speaker -> IO ()
closeSpeaker = mapM_ SDL.closeAudioDevice

-- Called by SDL from its audio thread whenever it needs more samples.
-- The phase carries the wave's position over from the previous buffer.
squareWave :: IORef Int -> Int -> Int16 -> SDL.AudioFormat sample -> MV.IOVector sample -> IO ()
squareWave phase halfPeriod amplitude format buffer = case format of
  SDL.Signed16BitLEAudio -> do
    let samples = MV.length buffer
    start <- readIORef phase
    forM_ [0 .. samples - 1] $ \i -> do
      let high = ((start + i) `div` halfPeriod) `mod` 2 == 0
      MV.write buffer i (if high then amplitude else negate amplitude)
    writeIORef phase ((start + samples) `mod` (2 * halfPeriod))
  _ -> error "Unsupported audio format"
