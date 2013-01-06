module Spotify.Error (
  Error(..),
  spotifyError
) where

import qualified Bindings.Spotify.Error as SP

data Error = Ok
           | ClientTooOld
           | UnableToContactServer
           | BadUsernameOrPassword
           | UserBanned
           | UserNeedsPremium

spotifyError :: SP.Error -> Error
spotifyError err | err == SP.ok                    = Ok
                 | err == SP.clientTooOld          = ClientTooOld
                 | err == SP.unableToContactServer = UnableToContactServer
                 | err == SP.userBanned            = UserBanned
                 | err == SP.userNeedsPremium      = UserNeedsPremium
