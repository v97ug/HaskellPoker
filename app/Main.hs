module Main where

import Lib

import Cards
import Hands

import Data.List
import qualified Data.Map as Map
import System.Random
import FreeGame

update :: Font -> [Card] -> [Bitmap] -> Map.Map Card Bitmap -> Bitmap -> Game()
update font myCards cards cardsMap back = do
  translate (V2 600 400) $ bitmap back
  -- showCards myCards 100 font
  showPictCards myCards cardsMap 100
  (translate (V2 100 600) . color green . text font 40) "exmple"
  let (Just hand) = toHand myCards
  let (yaku,c) = pokerHand hand
  (translate (V2 100 700) . color green . text font 40) $ show yaku
  (translate (V2 100 800) . color green . text font 40) $ show c
  tick
  escape <- keyPress KeyEscape
  unless escape $ update font myCards cards cardsMap back


main :: IO (Maybe ())
main = do
  gen <- newStdGen

  runGame Windowed (Box (V2 0 0) (V2 1200 800)) $ do
    -- clearColor $ fromRGB 0.36 0.66 0.29
    font <- loadFont "asset/VL-PGothic-Regular.ttf"
    -- c1 <- readBitmap "asset/cards/c01.png"
    back <- readBitmap "asset/back.png"
    cards <- mapM readBitmap  ["asset/cards/" ++ r ++ n ++ ".png" | r <- ["c","d", "h", "s"], n <- map (add0str . show) [1..13]]
    let cardsMap = Map.fromList $ tupleCards cards Clover 1

    let shuffled = shuffleCard allCards gen


    update font (sort (take 5 shuffled)) cards cardsMap back

add0str :: String -> String
add0str s
  | length s == 1 = '0' : s
  | otherwise = s
