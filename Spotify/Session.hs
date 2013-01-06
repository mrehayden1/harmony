module Spotify.Session (
  Session,
  SessionLoginError(..),
  SessionInitError(..),
  Password,
  Username,
  initialiseSession,
  loginUser,
  changeTrack,
  playCurrentTrack
) where

import Bindings.Spotify.Data (Session(..), SessionCallbacks(..),
         SessionConfig(..), Track(..), emptySessionCallbacks)
import Bindings.Spotify.Data.SessionCallbacks (LoginCallback,
         MusicDeliveryCallback)
import Bindings.Spotify.Data.SessionConfig (defaultSessionConfig)
import Bindings.Spotify.Error (Error)
import qualified Bindings.Spotify.Error as E
import Bindings.Spotify.Session (Credentials, nullCredentials,
         sp_session_create, sp_session_login, sp_session_player_load,
         sp_session_player_play, sp_session_process_events)

import Control.Concurrent
import Control.Concurrent.Chan

import Foreign hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)

import Console.Debug (putDebug)
import qualified Spotify.Appkey as Key (applicationKey, applicationKeySize)
import qualified Spotify.Config as Conf (cacheLocation, settingsLocation,
         userAgentString)
import Spotify.Audio (Audio(..), AudioFormat, initAudio)
import Spotify.Error (spotifyError)

data SessionInitError = BadApiVersion
                      | BadUserAgent
                      | BadApplicationKey
                      | ApiInitializationFailed
                      | InvalidDeviceId

initialiseSession :: IO (Either SessionInitError Session)
initialiseSession = do
  -- Create a channel for audio to be written to by Spotify and read by code
  -- that pushes it to ALSA
  audioChan <- newChan
  initAudio audioChan
  -- Include callbacks in the session in order to create a syncronous API
  loginCallback <- sessionLoginCallback
  musicDeliveryCallback <- sessionMusicDeliveryCallback audioChan
  let callbacks = emptySessionCallbacks {
    onLoggedIn = loginCallback,
    onMusicDelivery = musicDeliveryCallback
  }
  (err, sess) <- with callbacks $ \cbs -> do
    let config = sessionConfig { callbacks = cbs }
    with config $ \cnf ->
      alloca $ \sess -> do
        err <- sp_session_create cnf sess
        return (err, sess)
  case err of
    _ | err == E.ok                      -> Right `fmap` peek sess
      | err == E.badApiVersion           -> return $ Left BadApiVersion
      | err == E.badUserAgent            -> return $ Left BadUserAgent
      | err == E.badApplicationKey       -> return $ Left BadApplicationKey
      | err == E.apiInitializationFailed -> return $
                                              Left ApiInitializationFailed 
      | err == E.invalidDeviceId         -> return $ Left InvalidDeviceId

sessionConfig :: SessionConfig
{-# NOINLINE sessionConfig #-}
sessionConfig =
  unsafePerformIO $
    withCString Conf.cacheLocation $ \cacheLoc ->
      withCString Conf.settingsLocation $ \settngsLoc ->
        withCString Conf.userAgentString $ \usrAgentStr ->
          return $ defaultSessionConfig {
            cacheLocation = cacheLoc
          , settingsLocation = settngsLoc
          , applicationKey = Key.applicationKey
          , applicationKeySize = Key.applicationKeySize
          , userAgent = usrAgentStr
          }

type Username = String
type Password = String

data SessionLoginError = ClientTooOld
                       | UnableToContactServer
                       | BadUsernameOrPassword
                       | UserBanned
                       | UserNeedsPremium
                       | Unknown

loginResult :: MVar Error
{-# NOINLINE loginResult #-}
loginResult = unsafePerformIO newEmptyMVar

loginUser :: Session -> Username -> Password -> IO (Either SessionLoginError ())
loginUser sess username password = do
  withCString username $ \usr ->
    withCString password $ \pss -> do
      _ <- sp_session_login sess usr pss 0 nullCredentials
      waitForLogin
 where
  waitForLogin =
    alloca $ \timeout -> do
      _ <- sp_session_process_events sess timeout
      result <- tryTakeMVar loginResult
      case result of
        Nothing -> do
          threadDelay (500 * 1000)
          waitForLogin
        Just err -> return $
          case err of
            _ | err == E.ok                    -> Right ()
              | err == E.clientTooOld          -> Left ClientTooOld
              | err == E.unableToContactServer -> Left UnableToContactServer
              | err == E.badUsernameOrPassword -> Left BadUsernameOrPassword
              | err == E.userBanned            -> Left UserBanned
              | err == E.userNeedsPremium      -> Left UserNeedsPremium
              | otherwise                      -> Left Unknown

foreign import ccall safe "wrapper" wrapLoginCallback
  :: LoginCallback -> IO (FunPtr LoginCallback)

sessionLoginCallback :: IO (FunPtr LoginCallback)
sessionLoginCallback = wrapLoginCallback $ \_ err -> do
  putDebug "Spotify login callback called."
  putMVar loginResult err

foreign import ccall safe "wrapper" wrapMusicDeliveryCallback
  :: MusicDeliveryCallback -> IO (FunPtr MusicDeliveryCallback)

sessionMusicDeliveryCallback :: Chan Audio -> IO (FunPtr MusicDeliveryCallback)
sessionMusicDeliveryCallback chan = wrapMusicDeliveryCallback $
  \_ formatP samplesP samplesAvail -> do
    putDebug "Spotify music delivery callback called."
    smps <- peekArray (fromIntegral samplesAvail) samplesP
    format <- peek formatP
    writeChan chan $ Audio { audioFormat = format, samples = smps }
    return samplesAvail

-- TODO Expand if required
data PlayerLoadError = MissingPlayerCallback
                     | TrackNotPlayable
 deriving (Show)

changeTrack :: Session -> Track -> IO (Either PlayerLoadError ())
changeTrack sess tr = do
  err <- sp_session_player_load sess tr
  return $ case err of
    _ | err == E.ok               -> Right ()
      | err == E.missingCallback  -> Left MissingPlayerCallback
      | err == E.trackNotPlayable -> Left TrackNotPlayable

-- TODO Expand if required
data PlayerPlayError = PlayError

playCurrentTrack :: Session -> IO (Either PlayerPlayError ())
playCurrentTrack sess = do
  putDebug "Playing track."
  err <- sp_session_player_play sess 1
  if err == E.ok
    then return $ Right ()
    else return $ Left PlayError
