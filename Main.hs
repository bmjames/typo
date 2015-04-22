{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Main where

import Control.Applicative hiding ((<|>))
import Control.Arrow ((&&&))
import Control.Category (Category)
import Control.Exception (bracket)
import Control.Monad.Trans.Class (lift)
import Data.Int (Int64)
import Data.Foldable (foldMap)
import Data.Machine hiding (Z)
import Data.Monoid (mempty)
import Data.Ratio ((%))
import Data.Traversable
import Graphics.Vty
import System.Environment (getArgs)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

data Zipper a = Z ![a] ![a]

fromList :: [a] -> Zipper a
fromList (a:as) = Z [a] as

type ViewportState = Zipper (Maybe TL.Text, TL.Text)

data Stats = Stats { accuracy :: Maybe Rational }

main :: IO ()
main = do
  (f:_) <- getArgs
  copy <- TL.readFile f
  bracket (mkVty mempty) shutdown $ \vty -> do
    (initialW, _) <- displayBounds $ outputIface vty
    let chunkedCopy = TL.chunksOf (fromInt initialW - 2) copy
        vpState = fromList $ zip (Just "" : repeat Nothing) chunkedCopy
    runT_ $ events vty
            ~> handleEvents copy vpState
            ~> display vty

display :: Vty -> ProcessT IO Picture ()
display = autoM . update

events :: Vty -> SourceT IO Event
events vty = construct go where
  go = lift (nextEvent vty) >>= yield >> go

handleEvents :: TL.Text -> ViewportState -> Process Event Picture
handleEvents copy s = construct acceptChars ~> ui

  where
    acceptChars :: Category k => Plan (k Event) Char Event
    acceptChars = do
      event <- await
      case event of EvKey (KChar c) [] -> yield c >> acceptChars
                    EvKey KEsc      [] -> stop
                    _ -> acceptChars

    ui :: Process Char Picture
    ui = auto $ foldUI copy s

foldUI :: TL.Text -> ViewportState -> Moore Char Picture
foldUI copy s =
  fromLayers <$>
  sequenceA [ info <$> foldStats copy
            , foldViewport s ]

  where
    bg = Background ' ' (defAttr `withBackColor` black)
    fromLayers layers = (picForLayers layers) { picBackground = bg }

-- | Fold over the input, producing the text viewport image
foldViewport :: ViewportState -> Moore Char Image
foldViewport = unfoldMoore (drawViewport &&& updateState)

  where
    updateState :: ViewportState -> Char -> ViewportState
    updateState (Z ((Just curInput, curCopy):ls) rs) c =
      let curInput' = TL.snoc curInput c
          newLine = TL.length curInput' == TL.length curCopy
      in case rs of
        (Nothing, nextCopy) : rs' | newLine ->
          Z ((Just "", nextCopy) : (Just curInput', curCopy) : ls) rs'
        _ -> Z ((Just curInput', curCopy) : ls) rs

-- | Fold over the input, producing accuracy statistics
foldStats :: TL.Text -> Moore Char Stats
foldStats copyText = unfoldMoore (toStats &&& updateAccuracy) (copyText, 0, 0)

  where
    updateAccuracy (copy, g, n) c =
      case TL.uncons copy of
        Just (c', copy') -> if c == c'
                               then (copy', succ g, succ n)
                               else (copy', g, succ n)
        Nothing -> (copy, g, n)

    toStats (_, g, n) = Stats (if n == 0 then Nothing else Just $ g % n)

drawViewport :: ViewportState -> Image
drawViewport (Z ((Just curInput, curCopy):ls) rs) =
  pad 1 1 1 1 $ vertCat $
    interleave emptyLines $ history ++ activeLine ++ lookAhead

  where
    n = 3

    history = concatMap (\(Just input, copy) -> [text copyAttr copy, showDiffs copy input])
                (reverse $ take n ls)

    activeLine = [ withCursorAt (toInt $ TL.length curInput) curCopy
                 , showDiffs curCopy curInput ]

    lookAhead = interleave (map (text copyAttr . snd) (take (2*n) rs)) emptyLines

    emptyLines = repeat (backgroundFill 1 1)

showDiffs :: TL.Text -> TL.Text -> Image
showDiffs as bs = resizeHeight 1 $ horizCat $
  map (\(a, b) -> let attr = if a == b then goodInput else badInput in char attr b) $
  TL.zip as bs

withCursorAt :: Int -> TL.Text -> Image
withCursorAt i txt =
  let (left, right) = TL.splitAt (fromInt i) txt
  in text copyAttr left <|>
     foldMap (\(r, rs) -> char cursorAttr r <|> text copyAttr rs) (TL.uncons right)

fromInt :: Int -> Int64
fromInt = fromInteger . toInteger

toInt :: Int64 -> Int
toInt = fromInteger . toInteger

info :: Stats -> Image
info (Stats a) = string (defAttr `withForeColor` black `withBackColor` white)
                    ("typo - press Esc to exit" ++ foldMap showAccuracy a)
  where
    showAccuracy ratio = " - accuracy: " ++ show (truncate (ratio * 100) :: Int) ++ "%"

interleave :: [a] -> [a] -> [a]
interleave = fmap concat . zipWith (\x y -> [x, y])

copyAttr, goodInput, badInput, cursorAttr :: Attr
copyAttr = defAttr
goodInput = defAttr `withForeColor` cyan
badInput = defAttr `withForeColor` black `withBackColor` red
cursorAttr = copyAttr `withForeColor` black `withBackColor` white
