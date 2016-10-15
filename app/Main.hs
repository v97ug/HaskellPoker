module Main where

import Lib

import Cards

import Data.List
import qualified Data.Map as Map
import System.Random
import FreeGame

update :: Font -> [Card] -> [Bitmap] -> Map.Map Card Bitmap -> Game()
update font myCards cards cardsMap = do
  -- showCards myCards 100 font
  showPictCards myCards cardsMap 100

  tick
  escape <- keyPress KeyEscape
  unless escape $ update font myCards cards cardsMap


main :: IO (Maybe ())
main = do
  gen <- newStdGen

  runGame Windowed (Box (V2 0 0) (V2 1200 800)) $ do
    clearColor black
    font <- loadFont "asset/VL-PGothic-Regular.ttf"
    c1 <- readBitmap "asset/cards/c01.png"
    cards <- mapM readBitmap  ["asset/cards/" ++ r ++ n ++ ".png" | r <- ["c","d", "h", "s"], n <- map (add0str . show) [1..13]]
    let cardsMap = Map.fromList $ tupleCards cards Clover 1

    let shuffled = shuffleCard allCards gen

    update font (sort (take 5 shuffled)) cards cardsMap

add0str :: String -> String
add0str s
  | length s == 1 = '0' : s
  | otherwise = s
