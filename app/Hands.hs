module Hands
( Hand
, PokerHand
, toHand, fromHand
, pokerHand
) where

import Cards
import Data.List
import Control.Monad

newtype Hand = Hand {fromHand :: [Card] } deriving (Show, Eq, Ord)

toHand :: [Card] -> Maybe Hand
toHand l =
  if length l == 5
    then Just $ Hand (sort l)
    else Nothing

pokerHand :: Hand -> (PokerHand, Card)
pokerHand h@(Hand l) =
  case foldl mplus Nothing $ map (\f -> f h) hands of
    Just card -> card
    Nothing -> (HighCards, last l)
  where
    hands :: [Hand -> Maybe (PokerHand, Card)]
    hands =
      [straightFlush
      , fourCards
      , fullHouse
      , flush
      , straight
      , threeCards
      , twoPair
      , onePair
      ]

data PokerHand
  = HighCards
  | OnePair
  | TwoPair
  | ThreeCards
  | Straight
  | Flush
  | FullHouse
  | FourCards
  | StraightFlush
  deriving (Show, Read, Eq, Ord, Enum)

isStraight :: [Int] -> Bool
isStraight xs@(x:_) = xs == [x .. x + 4]
isStraight _ = False

judgeStraight :: Hand -> Maybe Card
judgeStraight (Hand h) =
  if isStraight $ map getNum h
    then Just (last h)
    else Nothing

isFlush :: Hand -> Maybe Card
isFlush (Hand (x:xs)) = -- なんで(Hand h)ってやるの〜？？
  if all ((getRank x ==) . getRank) xs
    then Just (last xs)
    else Nothing

isNPair :: Int -> Hand -> Maybe [[Card]] -- Handにすると、エラーする -- なんで(Hand h)ってやるの〜？？
isNPair n (Hand h) = if cards  /= [] then Just cards else Nothing
  where
    cards :: [[Card]]
    cards = filter ((==n).length) $ groupBy (\x y -> getNum x == getNum y) h

straightFlush :: Hand -> Maybe (PokerHand, Card)
straightFlush h = do
  st <- judgeStraight h
  isFlush h
  return (StraightFlush, st)

fourCards :: Hand -> Maybe (PokerHand, Card)
fourCards h = do
  cs <- isNPair 4 h
  return (FourCards, last $ concat cs)

fullHouse :: Hand -> Maybe (PokerHand, Card)
fullHouse h = do
  cs1 <- isNPair 3 h
  cs2 <- isNPair 2 h
  return (FullHouse, maximum $ concat cs1 ++ concat cs2)

flush :: Hand -> Maybe (PokerHand, Card)
flush h = do
  c <- isFlush h
  return (Flush, c)

straight :: Hand -> Maybe (PokerHand, Card)
straight h = do
  c <- judgeStraight h
  return (Straight, c)

threeCards :: Hand -> Maybe (PokerHand, Card)
threeCards h = do
  cs <- isNPair 3 h
  return (ThreeCards, last $ concat cs)

twoPair :: Hand -> Maybe (PokerHand, Card)
twoPair h = do
  cs <- isNPair 2 h
  if length cs == 2
    then Just (TwoPair, last $ concat cs)
    else Nothing

onePair :: Hand -> Maybe (PokerHand, Card)
onePair h = do
  cs <- isNPair 2 h
  return (OnePair, last $ concat cs)
