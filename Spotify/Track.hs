module Spotify.Track (
  getTrackName
) where

import Bindings.Spotify.Data (Track)
import Bindings.Spotify.Track (sp_track_name)

import Foreign.C.String

getTrackName :: Track -> IO String
getTrackName tr = sp_track_name tr >>= peekCString
