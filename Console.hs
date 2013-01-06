module Console (
  Arguments(..),
  getArguments,
  initialiseConsole,
  promptUsernameAndPassword,
  putErrorAndExit
) where

import Control.Applicative

import Data.List

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (BufferMode(..), hSetBuffering, hSetEcho, stdin, stdout)

import Console.Args (Arguments(..), parseArguments)
import Spotify.Session (Password, Username)

initialiseConsole :: IO ()
initialiseConsole = do
  hSetBuffering stdout NoBuffering

putErrorAndExit :: String -> IO a
putErrorAndExit msg = do
  putStrLn ("Error: " ++ msg)
  exitWith $ ExitFailure 1

getArguments :: IO Arguments
getArguments = do
  input <- intercalate " " `fmap` getArgs
  case parseArguments input of
    Nothing   -> putErrorAndExit "Error parsing arguments."
    Just args -> return args

promptUsernameAndPassword :: Arguments -> IO (Username, Password)
promptUsernameAndPassword args =
  (,) <$> maybe promptUsername (return . id) (username args)
      <*> maybe promptPassword (return . id) (password args)
 where
  promptUsername = putStr "Spotify username: " >> getLine

  promptPassword = do
    hSetEcho stdin False
    putStr "Password: "
    pass <- getLine
    putStrLn ""
    hSetEcho stdin True
    return pass
