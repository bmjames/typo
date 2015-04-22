{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative hiding ((<|>))
import Control.Exception (bracket)
import Control.Monad.RWS
import Data.Int (Int64)
import Data.Foldable (foldMap)
import Data.Ratio ((%))
import Data.Traversable
import Graphics.Vty
import System.Environment (getArgs)

import qualified Control.Foldl as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

data Zipper a = Z ![a] ![a]

fromList :: [a] -> Zipper a
fromList (a:as) = Z [a] as

type ViewportState = Zipper (Maybe TL.Text, TL.Text)

type App a = RWST Vty () (L.Fold (DisplayRegion, Char) Picture) IO a

data Stats = Stats { accuracy :: Maybe Rational }

main :: IO ()
main = do
  (f:_) <- getArgs
  copy <- TL.readFile f
  bracket (mkVty mempty) shutdown $ \vty -> do
    (initialW, _) <- displayBounds $ outputIface vty
    let chunkedCopy = TL.chunksOf (fromInt initialW - 2) copy
        vpState = fromList $ zip (Just "" : repeat Nothing) chunkedCopy
    void $ execRWST (vtyInteract False)
                    vty
                    (foldUI copy vpState)

vtyInteract :: Bool -> App ()
vtyInteract exit = do
  updateDisplay
  unless exit (handleNextEvent >>= vtyInteract)

updateDisplay :: App ()
updateDisplay = do
  vty <- ask
  L.Fold _ x g <- get
  let picture = g x
  liftIO $ update vty picture

handleNextEvent :: App Bool
handleNextEvent = do
  vty <- ask
  event <- liftIO $ nextEvent vty
  case event of
    EvKey (KChar c) [] -> handleInput c
    _                  -> return ()
  return $ event == EvKey KEsc []

handleInput :: Char -> App ()
handleInput c = do
  vty <- ask
  region <- displayBounds $ outputIface vty
  modify $ step (region, c)

fromInt :: Int -> Int64
fromInt = fromInteger . toInteger

toInt :: Int64 -> Int
toInt = fromInteger . toInteger

info :: Stats -> Image
info (Stats a) = string (defAttr `withForeColor` black `withBackColor` white)
                    ("typo - press Esc to exit" ++ foldMap showAccuracy a)
  where
    showAccuracy ratio = " - accuracy: " ++ show (truncate (ratio * 100)) ++ "%"

interleave :: [a] -> [a] -> [a]
interleave = fmap concat . zipWith (\x y -> [x, y])

copyAttr, goodInput, badInput, cursorAttr :: Attr
copyAttr = defAttr
goodInput = defAttr `withForeColor` cyan
badInput = defAttr `withForeColor` black `withBackColor` red
cursorAttr = copyAttr `withForeColor` black `withBackColor` white

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

foldUI :: TL.Text -> ViewportState -> L.Fold (DisplayRegion, Char) Picture
foldUI copy s =
  fromLayers <$>
  sequenceA [ info <$> L.premap snd (foldStats copy)
            , foldViewport s ]

  where
    bg = Background ' ' (defAttr `withBackColor` black)
    fromLayers layers = (picForLayers layers) { picBackground = bg }

-- | Fold over the input and display state, producing the text viewport image
foldViewport :: ViewportState -> L.Fold (DisplayRegion, Char) Image
foldViewport s = L.Fold updateState s drawViewport where
  updateState :: ViewportState -> (DisplayRegion, Char) -> ViewportState
  updateState (Z ((Just curInput, curCopy):ls) rs) (_, c) =
    let curInput' = TL.snoc curInput c
        newLine = TL.length curInput' == TL.length curCopy
    in case rs of
      (Nothing, nextCopy) : rs' | newLine ->
        Z ((Just "", nextCopy) : (Just curInput', curCopy) : ls) rs'
      _ -> Z ((Just curInput', curCopy) : ls) rs

-- | Fold over the input, producing accuracy statistics
foldStats :: TL.Text -> L.Fold Char Stats
foldStats copyText =
  L.Fold updateAccuracy
         (copyText, 0, 0)
         (\(_, g, n) -> Stats (if n == 0 then Nothing else Just $ g % n))

  where
    updateAccuracy :: (TL.Text, Integer, Integer) -> Char -> (TL.Text, Integer, Integer)
    updateAccuracy (copy, g, n) c =
      case TL.uncons copy of
        Just (c', copy') -> if c == c' then (copy', succ g, succ n) else (copy', g, succ n)
        Nothing -> (copy, g, n)

step :: a -> L.Fold a b -> L.Fold a b
step a (L.Fold f x g) = L.Fold f (f x a) g
