module Main (
  main
) where

import Control.Monad

import Console (getArguments, initialiseConsole, promptUsernameAndPassword,
         putErrorAndExit)
import Console.Args (Arguments(..))
import Console.Debug (putDebug, setDebug)

import Config.Playlist as Pl (playlistUri)
import Control.Error
import Spotify.Link (CreateLinkError(..), Link, createLink)
import Spotify.Playlist (getPlaylistName, loadPlaylist, playlistTrack)
import Spotify.Session (Password, Session, SessionLoginError(..),
         SessionInitError(..), Username, changeTrack, playCurrentTrack,
         initialiseSession, loginUser)
import Spotify.Track (getTrackName)

tryLogin :: Session -> Username -> Password -> IO ()
tryLogin sess user pass = do
  loginUser sess user pass `catchLeft` \err ->
    case err of
      ClientTooOld          -> putErrorAndExit "The version of libspotify being used is too old."
      UnableToContactServer -> putErrorAndExit "Unable to contact Spotify server."
      BadUsernameOrPassword -> putErrorAndExit "Invalid username or password."
      UserBanned            -> putErrorAndExit "This account is banned."
      UserNeedsPremium      -> putErrorAndExit "Please upgrade your account to Spotify Premium."
      _                     -> putErrorAndExit "Other unknown error."
  putDebug "Logged in."

tryInitSession :: IO Session
tryInitSession = do
  Right sess <- initialiseSession `catchLeft` \err -> do
    case err of
      BadApiVersion           ->
        putErrorAndExit "Session initialisation failed (Bad API version)."
      BadUserAgent            ->
        putErrorAndExit "Session initialisation failed (Bad user agent)."
      BadApplicationKey       ->
        putErrorAndExit "Session initialisation failed (Bad application key)."
      ApiInitializationFailed ->
        putErrorAndExit "Session initialisation failed."
      InvalidDeviceId         ->
        putErrorAndExit "Session initialisation failed (Invalid device ID)."
  return sess

main :: IO ()
main = do
  initialiseConsole
  args <- getArguments
  setDebug $ debug args
  putDebug "Loading harmony..."
  (username, password) <- promptUsernameAndPassword args
  putDebug "Initialising session."
  sess <- tryInitSession
  putDebug "Attempting login."
  tryLogin sess username password
  Right link <- (return $ createLink Pl.playlistUri) `catchLeft` \_ ->
    putErrorAndExit "Could not parse link."
  Right playlist <- loadPlaylist sess link `catchLeft` \_ ->
    putErrorAndExit "Could not load playlist."
  getPlaylistName playlist >>= (putStrLn . ("Playlist name: " ++))
  Right track <- playlistTrack sess playlist 0 `catchLeft` \_ ->
    putErrorAndExit "Could not load track."
  getTrackName track >>= (putStrLn . ("Track 0 name: " ++))
  Right _ <- changeTrack sess track `catchLeft` \err -> do
    print err
    putErrorAndExit "Could not load track for playing."
  Right _ <- playCurrentTrack sess `catchLeft` \_ ->
    putErrorAndExit "Could not play track."
  putDebug "Waiting."
  forever $ return ()
