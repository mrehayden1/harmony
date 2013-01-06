module Control.Error (
  catchLeft
) where

import Control.Monad

catchLeft
  :: (Functor m, Monad m)
  => m (Either e a)
  -> (e -> m a)
  -> m (Either e a)
catchLeft result act = do
  r <- result
  case r of
    r@(Right a) -> return r
    Left e      -> Right `fmap` act e
