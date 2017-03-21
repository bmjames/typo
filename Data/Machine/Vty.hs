{-# LANGUAGE Rank2Types #-}
module Data.Machine.Vty where

import Control.Category (Category)
import Control.Monad.Trans.Class
import Data.Machine
import Graphics.Vty

display :: Vty -> ProcessT IO Picture ()
display = autoM . update

events :: Vty -> SourceT IO Event
events = constM . nextEvent

-- | Stops the event stream as soon as 'key' is received.
withQuitKey :: (Category k, Monad m) => Key -> MachineT m (k Event) Event
withQuitKey key = construct go where
  go = do
    event <- await
    case event of EvKey k [] | k == key -> stop
                  e -> yield e >> go

--
-- General machines combinators

constM :: Monad m => m a -> SourceT m a
constM a = repeatedly $ lift a >>= yield

mappedMaybe :: (Category k, Monad m) => (i -> Maybe o) -> MachineT m (k i) o
mappedMaybe f = construct go where
  go = do
    el <- await
    case f el of Just x  -> yield x >> go
                 Nothing -> go
