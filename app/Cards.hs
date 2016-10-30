module Cards
( Rank(..)
, Card
, getRank
, getNum
, allCards
, shuffleCard
, showCards
, tupleCards
, showPictCards
-- , checkHand
) where

import FreeGame
import System.Random
import qualified Data.Map as Map
import Data.Maybe

data Rank = Clover | Dia | Heart | Spade deriving (Eq, Ord, Enum)
data Card = Card Int Rank deriving (Eq, Ord)

getRank :: Card -> Rank
getRank (Card _ r) = r

getNum :: Card -> Int
getNum (Card n _) = n

instance Show Card where
  show (Card i Heart) = "H" ++ showCardNumber i
  show (Card i Dia) = "D" ++ showCardNumber i
  show (Card i Clover) = "C" ++ showCardNumber i
  show (Card i Spade) = "S" ++ showCardNumber i

showCardNumber :: Int -> String
showCardNumber 13 = "K_"
showCardNumber 12 = "Q_"
showCardNumber 11 = "J_"
showCardNumber 10 = "10"
showCardNumber x = show x ++ "_"

allCards :: [Card]
allCards = [Card n rank | n <- [1..13], rank <- [Clover ..]]

tupleCards :: [Bitmap] -> Rank -> Int -> [(Card,Bitmap)]
tupleCards [] _ _ = []
tupleCards (x:xs) rank n
  | n > 13 = tupleCards (x:xs) (succ rank) 1
  | otherwise = (Card n rank, x) : tupleCards xs rank (n+1)

shuffleCard :: [Card] -> StdGen -> [Card]
shuffleCard [] _ = []
shuffleCard cards gen =
  let
    (index,newGen) = randomR (0, length cards - 1) gen :: (Int, StdGen)
  in cards !! index : shuffleCard (take index cards ++ drop (index + 1) cards) newGen

showCards :: [Card] -> Double -> Font -> Game()
showCards [] _ _ = return ()
showCards (c:cs) x font = do
  (translate (V2 x 100) . color green . text font 40) (show c)
  showCards cs (x + 100) font

-- card size is 200 x 300
showPictCards :: [Card] -> Map.Map Card Bitmap -> Double -> Game()
showPictCards [] _ _ = return ()
showPictCards (c:cs) cardsMap x
  | isJust pict = do
    translate (V2 x 210) . bitmap $ let (Just p) = pict in p
    showPictCards cs cardsMap (x + 200)
  | otherwise = showPictCards cs cardsMap (x + 200)
  where pict = Map.lookup c cardsMap

-- checkHand :: [Card] -> String
-- checkHand c = "aaa"
-- checkHand [Card 1 r, Card 10 r, Card 11 r, Card 12 r, Card 13 r] = "royal straight flash"
-- checkHand _ = "I'm sorry..."
