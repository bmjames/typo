{-# LANGUAGE OverloadedStrings #-}
module Main where

import Debug.Trace

import Control.Exception (bracket)
import Control.Monad.RWS
import Data.Int (Int64)
import Graphics.Vty
import System.Environment (getArgs)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

data AppConfig = AppConfig { _vty :: Vty, _copy :: TL.Text }

data Zipper a = Z ![a] ![a]

fromList :: [a] -> Zipper a
fromList (a:as) = Z [a] as

type AppState = Zipper (Maybe TL.Text, TL.Text)

type App a = RWST AppConfig () AppState IO a

main :: IO ()
main = do
  (f:_) <- getArgs
  copy <- TL.readFile f
  bracket (mkVty mempty) shutdown $ \vty -> do
    (initialW, _) <- displayBounds $ outputIface vty
    let chunkedCopy = TL.chunksOf (fromInt initialW - 2) copy
    void $ execRWST (vtyInteract False)
                    (AppConfig vty copy)
                    (fromList $ zip (Just "" : repeat Nothing) chunkedCopy)

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

copyAttr, goodInput, badInput, cursorAttr :: Attr
copyAttr = defAttr
goodInput = defAttr `withForeColor` cyan
badInput = defAttr `withForeColor` black `withBackColor` red
cursorAttr = copyAttr `withForeColor` black `withBackColor` white

showState :: Int -> AppState -> Image
showState width (Z ((Just curInput, curCopy):ls) rs) =
  pad 1 1 1 1 $ vertCat $
    interleave emptyLines $ history ++ activeLine ++ lookAhead

  where
    n = 3

    history = concatMap (\(Just input, copy) -> [text copyAttr copy, showDiffs copy input])
                (reverse $ take n ls)

    activeLine = [ withCursorAt (toInt $ TL.length curInput) curCopy
                 , showDiffs curCopy curInput ]

    lookAhead = interleave (map (text copyAttr . snd) (take (2*n) rs)) emptyLines

    emptyLines = repeat (backgroundFill width 1)

showDiffs :: TL.Text -> TL.Text -> Image
showDiffs as bs = resizeHeight 1 $ horizCat $
  map (\(a, b) -> let attr = if a == b then goodInput else badInput in char attr b) $
  TL.zip as bs

withCursorAt :: Int -> TL.Text -> Image
withCursorAt i txt =
  let (left, right) = TL.splitAt (fromInt i) txt
      Just (r, rs) = TL.uncons right
  in text copyAttr left <|> char cursorAttr r <|> text copyAttr rs

updateDisplay :: App ()
updateDisplay = do
  vty <- asks _vty
  (w, _) <- displayBounds $ outputIface vty
  s <- get
  let bg = Background ' ' (defAttr `withBackColor` black)
      img = (picForLayers [info, showState w s]) { picBackground = bg }
  liftIO $ update vty img

pushChar :: Char -> App ()
pushChar c = do
  Z ((Just curInput, curCopy):ls) (next@(Nothing, nextCopy):rs) <- get
  let curInput' = TL.snoc curInput c
      newLine = TL.length curInput' == TL.length curCopy
      s' = if newLine
              then Z ((Just "", nextCopy) : (Just curInput', curCopy) : ls) rs
              else Z ((Just curInput', curCopy) : ls) (next : rs)
  put s'

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
