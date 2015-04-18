{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.RWS
import Data.Text.Lazy (Text)
import Graphics.Vty

import qualified Data.Text.Lazy as T

data AppConfig = AppConfig { _vty :: Vty, copy :: Text }

data AppState = AppState Text

type App a = RWST AppConfig () AppState IO a

main :: IO ()
main = do
  vty <- mkVty mempty
  execRWST (vtyInteract False) (AppConfig vty "foo bar") (AppState mempty)
  shutdown vty

vtyInteract :: Bool -> App ()
vtyInteract exit = do updateDisplay
                      unless exit (handleNextEvent >>= vtyInteract)

info :: Image
info = string (defAttr `withForeColor` black `withBackColor` white)
              "typo - press Esc to exit"

textArea :: Image
textArea = string defAttr ""

updateDisplay :: App ()
updateDisplay = do vty <- asks _vty
                   liftIO $ update vty $ picForImage (info <-> textArea)

handleNextEvent :: App Bool
handleNextEvent = do vty <- asks _vty
                     event <- liftIO $ nextEvent vty
                     return $ event == EvKey KEsc []
