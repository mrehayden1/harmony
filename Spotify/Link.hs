module Spotify.Link (
  CreateLinkError(..),
  Link,
  createLink
) where

import Bindings.Spotify.Data (Link(..))
import Bindings.Spotify.Link (sp_link_create_from_string)

import Foreign hiding (unsafePerformIO)
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)

data CreateLinkError = ParseError

createLink :: String -> Either CreateLinkError Link
{-# NOINLINE createLink #-}
createLink uri = unsafePerformIO . withCString uri $ \u -> do
  let l = sp_link_create_from_string u
  return $ if unLink l == nullPtr
             then Left ParseError
             else Right l
