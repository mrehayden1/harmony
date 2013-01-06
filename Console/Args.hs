module Console.Args (
  Arguments (
    username
  , password
  , debug
  )
, parseArguments
) where

import Control.Applicative

import Data.Char (isSpace)
import Data.Monoid (Dual(..), Endo(..), Monoid, mconcat)

import Text.Parsec (anyChar, choice, eof, many1, runParser, satisfy, sepBy, space,
  spaces, string, try)
import Text.Parsec.String (Parser)

import Spotify.Session (Password, Username)

data Arguments = Arguments {
  username :: Maybe Username
, password :: Maybe Password
, debug    :: Bool
}
 deriving (Show, Eq)

defaultArguments :: Arguments
defaultArguments = Arguments {
  username = Nothing,
  password = Nothing,
  debug    = False
}

parseArguments :: String -> Maybe Arguments
parseArguments input =
  case runParser argumentsP () "" input of
    Left _     -> Nothing
    Right opts -> Just $ appOptions opts defaultArguments


newtype Options = Options { unOption :: Dual (Endo Arguments) }
 deriving (Monoid)

option :: (Arguments -> Arguments) -> Options
option = Options . Dual . Endo

appOptions :: Options -> Arguments -> Arguments
appOptions = appEndo . getDual . unOption


notSpace :: Parser Char
notSpace = satisfy (not . isSpace)

optionArgumentP :: Parser String
optionArgumentP = many1 notSpace

argumentsP :: Parser Options
argumentsP = do
  spaces
  opts <- choice [
      try userOptionP
    , try passwordOptionP
    , try debugOption
    ] `sepBy` many1 space
  eof
  return $ mconcat opts

userOptionP :: Parser Options
userOptionP = string "-u" *> (updateUsername <$> optionArgumentP)
 where
  updateUsername u = option $ \a -> a { username = Just u }

passwordOptionP :: Parser Options
passwordOptionP = string "-p" *> (updatePassword <$> optionArgumentP)
 where
  updatePassword p = option $ \a -> a { password = Just p }

debugOption :: Parser Options
debugOption =  enableDebug <$ string "-d"
 where
  enableDebug = option $ \a -> a { debug = True }
