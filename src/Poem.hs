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
  result = mconcat [buildCouplet <$> s!!(n-c1) <*> s!!(n-c2) <*> cp
    | ((c1, c2), cp) <- Map.assocs pairsByCounts]
  buildCouplet l1 l2 (r1, r2) = (l1 ++ [r1], l2 ++ [r2])
  n = length pattern
  s = lineChoiceSeq dictionary pattern
  pairsByCounts = listToChoice <$> keyBy (syllableCount *** syllableCount) rhymePairs
  syllableCount = length . stressPattern
  rhymePairs = [(p1, p2) | rs <- rhymes, p1 <- rs, p2 <- rs, p1 /= p2]
  rhymes = rhymeSets endWords
  endWords :: [Pronunciation]
  endWords = [p | p <- dictionary, stressPattern p `isSuffixOf` pattern]
