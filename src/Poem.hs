{-# LANGUAGE RankNTypes #-}
module Poem
  ( rhymeSets
  , lineChoice
  , lineChoiceSeq
  , coupletChoice
  ) where

import Control.Arrow
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (isSuffixOf, inits, tails)
import System.Random

import Choice
import Pronunciation

-- Turns a list of values into a map indexed by the given function.
keyBy :: (Ord k) => (v -> k) -> [v] -> Map k [v]
keyBy f vs = Map.fromListWith (++) $ map (f &&& (\v -> [v])) vs

-- Groups Pronunciations by their 'rhymeStem's.
rhymeSets :: [Pronunciation] -> [[Pronunciation]]
rhymeSets = Map.elems . keyBy rhymeStem

-- Given a dictionary and a line meter make a choice object over all lines
-- conforming to the meter.
lineChoice :: [Pronunciation] -> [Bool] -> Choice [Pronunciation]
lineChoice dictionary pattern = last $ lineChoiceSeq dictionary pattern

lineChoiceSeq :: [Pronunciation] -> [Bool] -> [Choice [Pronunciation]]
lineChoiceSeq dictionary pattern = resultList where
  wordChoiceByStressMap = listToChoice <$> keyBy stressPattern dictionary
  wordChoice :: [Bool] -> Choice Pronunciation
  wordChoice sp = maybe mempty id (Map.lookup sp wordChoiceByStressMap)
  transitions n = filter (/=[]) $ tails $ take n pattern
  resultList = fmap result [0..length pattern]
  result :: Int -> Choice [Pronunciation]
  result 0 = listToChoice [[]]
  result n = mconcat
    [(\x y -> x ++ [y]) <$> (resultList !! (n - length t)) <*> wordChoice t
      | t <- transitions n]


coupletChoice :: [Pronunciation] -> [Bool] -> Choice ([Pronunciation], [Pronunciation])
coupletChoice dictionary pattern = result where
  result = mconcat [buildCouplet <$> s!!r1 <*> s!!r2 <*> cp
    | ((r1, r2), cp) <- Map.assocs pairChoiceByRemainders]
  buildCouplet l1 l2 (r1, r2) = (l1 ++ [r1], l2 ++ [r2])
  s = lineChoiceSeq dictionary pattern
  pairChoiceByRemainders =
    listToChoice <$> keyBy (remainder *** remainder) rhymePairs
  remainder = (n-) . length . stressPattern
  n = length pattern
  rhymePairs = [(x, y) | r <- rhymes, x <- r, y <- r, x /= y]
  rhymes = rhymeSets endWords
  endWords :: [Pronunciation]
  endWords = [p | p <- dictionary, stressPattern p `isSuffixOf` pattern]
