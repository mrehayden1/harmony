module Spotify.Appkey (
  applicationKey,
  applicationKeySize
) where

import Bindings.Spotify.Data (ApplicationKey(..), ApplicationKeySize)

import Foreign hiding (unsafePerformIO)

import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "&g_appkey"
  g_appkey      :: ApplicationKey

foreign import ccall unsafe "&g_appkey_size"
  g_appkey_size :: Ptr ApplicationKeySize

applicationKey :: ApplicationKey 
applicationKey = g_appkey

applicationKeySize :: ApplicationKeySize
{-# NOINLINE applicationKeySize #-}
applicationKeySize = unsafePerformIO $ peek g_appkey_size 
