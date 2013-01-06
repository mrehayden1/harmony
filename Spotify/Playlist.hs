module Spotify.Playlist (
  getPlaylistName,
  loadPlaylist,
  playlistTrack
) where

import Debug.Trace

import Bindings.Spotify.Data (Link, Playlist(..), Session, Track)
import Bindings.Spotify.Data.PlaylistCallbacks (PlaylistCallbacks(..),
         PlaylistStateChangedCallback, emptyPlaylistCallbacks)
import Bindings.Spotify.Playlist (sp_playlist_add_callbacks, sp_playlist_create,
         sp_playlist_is_loaded, sp_playlist_name, sp_playlist_remove_callbacks,
         sp_playlist_track)
import Bindings.Spotify.Session (sp_session_process_events)
import Bindings.Spotify.Track (sp_track_error, sp_track_is_loaded)

import Control.Concurrent
import Control.Monad

import Data.Maybe

import Foreign hiding (unsafePerformIO)
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)

import Console.Debug (putDebug)

playlistLoaded :: MVar ()
playlistLoaded = unsafePerformIO newEmptyMVar

playlistStateChangedCallback :: FunPtr PlaylistStateChangedCallback
{-# NOINLINE playlistStateChangedCallback #-}
playlistStateChangedCallback = unsafePerformIO $
  wrapPlaylistStateChangedCallback $ \pl _ -> do
    loaded <- (> 0) `fmap` sp_playlist_is_loaded pl 
    putDebug "Playlist status changed."
    when loaded $ do
      putDebug "Playlist is loaded."
      tryPutMVar playlistLoaded ()
      return ()

foreign import ccall "wrapper" wrapPlaylistStateChangedCallback
  :: PlaylistStateChangedCallback
  -> IO (FunPtr PlaylistStateChangedCallback)

-- TODO Expand
data PlaylistLoadError = Timeout

-- FIXME Load in with ForeignPtr eventually so the memory will be freed
-- | Syncronously load a spotify playlist
loadPlaylist
  :: Session
  -> Link
  -> IO (Either PlaylistLoadError Playlist)
loadPlaylist sess link = do
  pl <- sp_playlist_create sess link
  with callbacks $ \cbs ->
    sp_playlist_add_callbacks pl cbs nullPtr
  waitForList pl 0
 where
  callbacks = emptyPlaylistCallbacks {
      onPlaylistStateChanged = playlistStateChangedCallback
    }

  waitForList pl t | t > 5 * 1000 * 1000 = return $ Left Timeout
                   | otherwise           = do
    loaded <- tryTakeMVar playlistLoaded
    if isJust loaded
      then with callbacks $ \cbs -> do
             sp_playlist_remove_callbacks pl cbs nullPtr
             return $ Right pl
      else alloca $ \timeout -> do
             _ <- sp_session_process_events sess timeout
             threadDelay $ 500 * 1000
             waitForList pl (t + 500 * 1000)


getPlaylistName :: Playlist -> IO String
getPlaylistName pl = sp_playlist_name pl >>= peekCString


trackLoaded :: MVar ()
{-# NOINLINE trackLoaded #-}
trackLoaded = unsafePerformIO newEmptyMVar

-- TODO Expand
data PlaylistTrackError = Error

playlistTrack :: Session -> Playlist -> Int
  -> IO (Either PlaylistTrackError Track)
playlistTrack sess pl i = do
  tr <- sp_playlist_track pl (fromIntegral i)
  waitForTrack tr
 where
  waitForTrack tr = alloca $ \timeout -> do
    loaded <- (> 0) `fmap` sp_track_is_loaded tr
    _ <- sp_session_process_events sess timeout
    if loaded
      then return $ Right tr
      else (threadDelay 100) >> waitForTrack tr
