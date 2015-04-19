{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracket)
import Control.Monad.RWS
import Data.Int (Int64)
import Graphics.Vty
import System.Environment (getArgs)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

data AppConfig = AppConfig { _vty :: Vty, _copy :: TL.Text }

data AppState = AppState { _input :: ![String], _copyZipper :: Zipper TL.Text }

data Zipper a = Z ![a] ![a]

type App a = RWST AppConfig () AppState IO a

main :: IO ()
main = do
  (f:_) <- getArgs
  copy <- TL.readFile f
  bracket (mkVty mempty) shutdown $ \vty -> do
    (initialW, _) <- displayBounds $ outputIface vty
    let chunkedCopy = TL.chunksOf (fromInt initialW) copy
    void $ execRWST (vtyInteract False)
                    (AppConfig vty copy)
                    (AppState [""] (Z [] chunkedCopy))

fromInt :: Int -> Int64
fromInt = fromInteger . toInteger

toInt :: Int64 -> Int
toInt = fromInteger . toInteger

vtyInteract :: Bool -> App ()
vtyInteract exit = do
  updateDisplay
  unless exit (handleNextEvent >>= vtyInteract)

info :: Image
info = string (defAttr `withForeColor` black `withBackColor` white)
              "typo - press Esc to exit"

interleave :: [a] -> [a] -> [a]
interleave = fmap concat . zipWith (\x y -> [x, y])

showState :: Int -> Int -> AppState -> Image
showState w h (AppState input (Z ls rs)) =
  vertCat $ interleave emptyLines $ history ++ activeLine ++ lookAhead

  where
    n = 4

    history = interleave (map (text defAttr) (reverse $ take (pred n) ls))
                         (map (string inputAttr . reverse) (reverse $ drop 1 $ take n input))
    activeLine = [ withCursorAt (length $ head input) (head rs)
                 , string inputAttr $ reverse $ head input ]
    lookAhead = interleave (map (text defAttr) (take n $ drop 1 rs)) emptyLines

    emptyLines = repeat (string defAttr "")
    inputAttr = defAttr `withForeColor` green

    withCursorAt :: Int -> TL.Text -> Image
    withCursorAt i txt =
      let (left, right) = TL.splitAt (fromInt i) txt
          Just (r, rs) = TL.uncons right
      in text defAttr left <|> char cursorAttr r <|> text defAttr rs

    cursorAttr = defAttr `withForeColor` black `withBackColor` white

updateDisplay :: App ()
updateDisplay = do
  vty <- asks _vty
  (w, h) <- displayBounds $ outputIface vty
  s <- get
  let img = picForImage (info <-> showState w h s)
  liftIO $ update vty img

pushChar :: Char -> App ()
pushChar c = do
  AppState (i:is) z@(Z ls (r:rs)) <- get
  let newLine = succ (length i) == toInt (TL.length r)
  let input' = ["" | newLine] ++ (c:i):is
  let z' = if newLine then Z (r:ls) rs else z
  put $ AppState input' z'

dropChar :: App ()
dropChar = return ()

handleNextEvent :: App Bool
handleNextEvent = do
  vty <- asks _vty
  event <- liftIO $ nextEvent vty
  case event of
    EvKey (KChar c) []   -> pushChar c
    EvKey KBS []         -> dropChar
    _                    -> return ()
  return $ event == EvKey KEsc []
