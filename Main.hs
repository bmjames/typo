{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Main where

import           Control.Arrow ((&&&))
import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad (guard)

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Machine hiding (Z, zipWith)
import           Data.Machine.Vty
import           Data.Monoid
import           Data.Ratio ((%))
import           Data.Time
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as TL
import qualified Data.Text.Lazy.IO                  as TL

import           Graphics.Vty

import           System.Environment (getArgs)


data Z a = Z !(NonEmpty (a, a)) ![(Maybe a, a)]
type ViewportState = Z T.Text

initViewport :: [T.Text] -> ViewportState
initViewport (a:as) = Z (("", a) :| []) (zip (repeat Nothing) as)
initViewport [] = error "initViewport of empty list"

data Stats = Stats { statsProgress :: Int, statsAccuracy :: Maybe Int, statsWPM :: Maybe Int }

main :: IO ()
main = do
  (f:_) <- getArgs
  copy <- TL.readFile f
  bracket (mkVty mempty) shutdown $ \vty -> do
    (initialW, _) <- displayBounds $ outputIface vty
    let chunkedCopy = addRetSymbols
          $ map TL.toStrict
          $ concatMap (TL.chunksOf (fromIntegral initialW - 3))
                                   (TL.split (== '\n') copy)
        vpState = initViewport chunkedCopy
    runT_ $ events vty
            ~> withQuitKey KEsc
            ~> handleEvents (TL.fromChunks chunkedCopy) vpState
            ~> display vty

-- | Append return symbols to all but the last line
addRetSymbols :: [T.Text] -> [T.Text]
addRetSymbols = over (_init.mapped) (\x -> T.snoc x '⏎')

currentTime :: SourceT IO UTCTime
currentTime = constM getCurrentTime

timedEvents :: ProcessT IO a (a, UTCTime)
timedEvents = capR currentTime (repeatedly $ zipWithT (,))

handleEvents :: TL.Text -> ViewportState -> ProcessT IO Event Picture
handleEvents copy s = charEvents ~> timedEvents ~> ui where

    charEvents :: Process Event Char
    charEvents = mappedMaybe $ \event -> case event of
        EvKey (KChar c) [] -> Just c
        EvKey KEnter    [] -> Just '\n'
        _ -> Nothing

    ui :: Process (Char, UTCTime) Picture
    ui = auto $ foldUI copy s

foldUI :: TL.Text -> ViewportState -> Moore (Char, UTCTime) Picture
foldUI copy s =
  fromLayers <$>
  sequenceA [ info <$> foldStats copy, lmap fst $ foldViewport s ]

  where
    bg = Background ' ' (defAttr `withBackColor` black)
    fromLayers layers = (picForLayers layers) { picBackground = bg }

-- | Fold over the input, producing the text viewport image
foldViewport :: ViewportState -> Moore Char Image
foldViewport = unfoldMoore (drawViewport &&& updateState)

  where
    updateState :: ViewportState -> Char -> ViewportState
    updateState (Z ((curInput, curCopy) :| ls) rs) c =
      let curInput' = T.snoc curInput c
          newLine = T.length curInput' == T.length curCopy
      in case rs of
        (Nothing, nextCopy) : rs' | newLine ->
          Z (("", nextCopy) :| (curInput', curCopy) : ls) rs'
        _ -> Z ((curInput', curCopy) :| ls) rs

-- | Fold over the input, producing accuracy and speed statistics
foldStats :: TL.Text -> Moore (Char, UTCTime) Stats
foldStats copy = Stats <$> foldCount <*> foldAccuracy copy <*> foldWPM

foldCount :: Moore a Int
foldCount = dimap (const (Sum 1)) getSum logMoore

foldAccuracy :: TL.Text -> Moore (Char, UTCTime) (Maybe Int)
foldAccuracy copy = unfoldMoore (toAccuracy &&& updateState) (copy, 0, 0)

  where
    updateState (copy, g, n) (c,_) =
      case TL.uncons copy of
        Just (c', copy') -> (copy', if acceptInput c' c then succ g else g, succ n)
        Nothing -> (copy, g, n)

    toAccuracy (_, g, n) = guard (n /= 0) >> Just (truncate $ 100 *  g % n)

foldWPM :: Moore (Char, UTCTime) (Maybe Int)
foldWPM = unfoldMoore (toWpm &&& updateState) (0, Nothing, False) where

  updateState :: (Int, Maybe (UTCTime, UTCTime), Bool)
              -> (Char, UTCTime)
              -> (Int, Maybe (UTCTime, UTCTime), Bool)
  updateState (wc, Nothing, onSpace) (_, t') = (wc, Just (t', t'), onSpace)
  updateState (wc, Just (t0, _), onSpace) (c, t') =
    let onSpace' = c == ' ' || c == '\n'
        wc' = if onSpace' && not onSpace then succ wc else wc
    in (wc', Just (t0, t'), onSpace')

  toWpm :: (Int, Maybe (UTCTime, UTCTime), Bool) -> Maybe Int
  toWpm (wc, ts, _) = do
    (t0, t) <- ts
    guard (t0 /= t)
    return $ truncate $ toRational (60 * wc) / toRational (diffUTCTime t t0)

drawViewport :: ViewportState -> Image
drawViewport (Z ((curInput, curCopy) :| ls) rs) =
  pad 1 1 1 1 $ vertCat $
    interleave emptyLines $ history ++ activeLine ++ lookAhead

  where
    n = 3

    history = concatMap (\(input, copy) -> [text' copyAttr copy, showDiffs copy input])
                (reverse $ take n ls)

    activeLine = [ withCursorAt (T.length curInput) curCopy
                 , showDiffs curCopy curInput ]

    lookAhead = interleave (map (text' copyAttr . snd) (take (2*n) rs)) emptyLines

    emptyLines = repeat (backgroundFill 1 1)

showDiffs :: T.Text -> T.Text -> Image
showDiffs as bs = resizeHeight 1 $ horizCat $
  map (\(a, b) -> let attr = if acceptInput a b then goodInput else badInput in char attr (displayInput b)) $
  T.zip as bs

acceptInput :: Char -> Char -> Bool
acceptInput '⏎' '\n' = True
acceptInput expected actual = expected == actual

displayInput :: Char -> Char
displayInput '\n' = ' '
displayInput c = c

withCursorAt :: Int -> T.Text -> Image
withCursorAt i txt =
  let (left, right) = T.splitAt i txt
  in text' copyAttr left <|>
     foldMap (\(r, rs) -> char cursorAttr r <|> text' copyAttr rs) (T.uncons right)

info :: Stats -> Image
info (Stats p a wpm) =
    string (defAttr `withForeColor` black `withBackColor` white)
           ("typo - press Esc to exit" ++ showProgress ++ foldMap showAccuracy a ++ foldMap showWpm wpm)
  where
    showAccuracy a = " - accuracy: " ++ show a ++ "%"
    showWpm wpm = " - WPM: " ++ show wpm
    showProgress = " - progress: " ++ show p

interleave :: [a] -> [a] -> [a]
interleave = fmap concat . zipWith (\x y -> [x, y])

copyAttr, goodInput, badInput, cursorAttr :: Attr
copyAttr = defAttr
goodInput = defAttr `withForeColor` cyan
badInput = defAttr `withForeColor` black `withBackColor` red
cursorAttr = copyAttr `withForeColor` black `withBackColor` white
