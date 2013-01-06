module Spotify.Config (
  cacheLocation,
  settingsLocation,
  userAgentString
) where

cacheLocation :: FilePath
cacheLocation = "/var/tmp/harmony/cache"

settingsLocation :: FilePath
settingsLocation = "~/.harmony/settings"

userAgentString :: String
userAgentString = "harmony 0.1"
