{-# LANGUAGE Rank2Types #-}
module Data.Machine.Vty where

import Control.Monad.Trans.Class
import Data.Machine
import Graphics.Vty

display :: Vty -> ProcessT IO Picture ()
display = autoM . update

events :: Vty -> SourceT IO Event
events = constM . nextEvent

constM :: Monad m => m a -> SourceT m a
constM a = repeatedly $ lift a >>= yield
