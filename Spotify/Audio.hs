module Spotify.Audio (
  Audio(..),
  AudioFormat(..),
  initAudio
) where

import Bindings.Spotify.Data.AudioFormat (AudioFormat(..), Sample)

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad

import Console.Debug

data Audio = Audio {
  audioFormat :: AudioFormat,
  samples :: [Sample]
}

initAudio :: Chan Audio -> IO ()
initAudio chan = do
  _ <- forkIO . forever $
         do _ <- readChan chan
            putDebug "Read samples from chan."
  putDebug "Audio initialised."
  return ()
