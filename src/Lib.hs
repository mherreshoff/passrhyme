{-# LANGUAGE RankNTypes #-}
module Lib
  ( prefixOf
  , suffixOf
  , pairsFromSets 
  , tailsWhere
  , choose
  , Pronunciation(Pronunciation)
  , word, phonemes
  , readDictionaryFile
  , parseDictionaryFile
  , nonAlternate
  , stressPattern
  , rhymeStem
  , rhymeSets
  , countLines
  , countLinesWithIntermediates
  , constructLine
  ) where

import Control.Arrow
import qualified Data.Map as Map
import Data.Char (toLower, isNumber)
import Data.Map (Map)
import Data.List (inits, tails)
import System.Random

----- Generic Utility Code:

-- Checks whether the second list starts with the first list.
prefixOf :: (Eq a) => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

-- Checks whether the second list ends with the first list.
suffixOf :: (Eq a) => [a] -> [a] -> Bool
suffixOf xs ys = prefixOf (reverse xs) (reverse ys)

-- All pairs of elements drawn from the same sub-list.
pairsFromSets :: forall a . [[a]] -> [(a,a)]
pairsFromSets = concatMap pairs where
  pairs :: [a] -> [(a,a)]
  pairs [] = []
  pairs (x:xs) = map (\x' -> (x, x')) xs ++ pairs xs

-- All non-empty suffixes of a list.
nonEmptyTails :: [a] -> [[a]]
nonEmptyTails = tailsWhere (const True)

-- All (non-empty) suffixes of a list where the first element satisfies the given predicate.
tailsWhere :: (a -> Bool) -> [a] -> [[a]]
tailsWhere pred = iter where
  iter [] = []
  iter t@(x:xs) = (if pred x then (t:) else id) $ iter xs

-- Turns a list of values into a map indexed by the given function.
keyBy :: (Ord k) => (v -> k) -> [v] -> Map k [v]
keyBy f vs = Map.fromListWith (++) $ map (f &&& (\v -> [v])) vs

-- Pick a random element from a list.
choose :: (RandomGen g) => [a] -> g -> (a, g)
choose xs g = (x, g') where
  (idx, g') = randomR (0, length xs - 1) g
  x = xs !! idx

----- CMU Dictionary Utils

-- An entry from the CMU Pronuncing Dictionary.
data Pronunciation = Pronunciation
  { word :: String
  , phonemes :: [String] }
  deriving (Eq, Ord, Show)

-- Given a file name, produces a list of Pronunciation entries.
readDictionaryFile :: FilePath -> IO [Pronunciation]
readDictionaryFile f = do 
  s <- readFile f
  return $ parseDictionaryFile s

-- Turns the contents of the CMU Pronouncing Dictionary into a list of Pronunciation entries.
parseDictionaryFile :: String -> [Pronunciation]
parseDictionaryFile x = map parseEntry $ filter notComment $ lines x where
  notComment s = not (null s) && head s /= ';'
  parseEntry line = Pronunciation (map toLower word) phonemes where
    (word:phonemes) = words line

-- Weeds out alternate pronunciations (eg. "foo(1)").
nonAlternate :: Pronunciation -> Bool
nonAlternate p = last (word p) /= ')'

-- Reads the numeric tags the CMU dictionary puts on its vowels and returns a list (one bool per syllable) with True indicating
-- a stressed syllable and False indicating an unstressed syllable.
stressPattern :: Pronunciation -> [Bool]
stressPattern p = pattern where
  pattern = map (/='0') $ filter isNumber $ map last $ phonemes p

-- Checks whether a string is a stressed vowel.
isStressedVowel :: String -> Bool
isStressedVowel s = c == '1' || c == '2' where c = last s

-- Returns all phonemes in the Pronunciation starting from the last stressed vowel.
rhymeStem :: Pronunciation -> [String]
rhymeStem p = if null t then phonemes p else last t where
 t = tailsWhere isStressedVowel $ phonemes p

-- Groups Pronunciations by their 'rhymeStem's.
rhymeSets :: [Pronunciation] -> [[Pronunciation]]
rhymeSets = Map.elems . keyBy rhymeStem

-- Returns the number of possible lines given a dictionary and a meter.
countLines :: [Pronunciation] -> [Bool] -> Integer
countLines dictionary pattern = last $ countLinesWithIntermediates dictionary pattern

-- The number of possible lines given a dictionary and a meter, for each prefix of the meter.
countLinesWithIntermediates :: [Pronunciation] -> [Bool] -> [Integer]
countLinesWithIntermediates dictionary pattern = resList where
  dictCountsByStress = fromIntegral.length <$> keyBy stressPattern dictionary
  count sp = maybe 0 id (Map.lookup sp dictCountsByStress)
    -- How many words are there for each stress pattern?
  resList :: [Integer]
  resList = [1] ++ [res n | n <- [1..length pattern]]
    -- How many possible lines are there for each prefix of the meter (the result.)
    -- There's one empty line, and after that we recurse.
  transitions n = nonEmptyTails $ take n pattern
    -- The list of possible stress patterns for a word whose last syllable lands on syllable n.
  res n = sum [resList !! (n-length t) * count t | t <- transitions n]
    -- To count the number of n-syllable line-starts, we just need to sum over the possible
    -- syllable counts of the last word.  (Then you have 'count t' choices for the last word
    -- and 'resList !! (n-length t)' choices for the previous words in the line.)

-- Given an index at least zero, and less than countLines (on the same dictionary and meter) select
-- the pass-rhyme with that index.
--
-- Indices are interpreted as "little-endian", with the last word being the most significant
-- component of the index.
constructLine :: [Pronunciation] -> [Bool] -> Integer -> [Pronunciation]
constructLine dictionary pattern index = iter (length pattern) index where
  wordsByStressMap = keyBy stressPattern dictionary
  wordsByStress sp = maybe [] id (Map.lookup sp wordsByStressMap)
    -- Which words are there for each stress pattern?
  counts = countLinesWithIntermediates dictionary pattern
    -- possibility counts for each prefix of the pattern.
  transitions n = nonEmptyTails $ take n pattern
    -- The list of possible stress patterns for a word whose last syllable lands on syllable n.
  iter :: Int -> Integer -> [Pronunciation]
  iter 0 _ = []
    -- If we've hit the beginning of the line, we're done.
  iter n idx = iter n' idx' ++ [lastWord] where
    possibleLastWords = concatMap wordsByStress $ transitions n
    numPossibilitiesGivenLastWord =
      [counts !! (n - length (stressPattern p)) | p <- possibleLastWords]
    startingIndexGivenLastWord = scanl (+) 0 numPossibilitiesGivenLastWord 
      -- We're assigning each possible last word of the poem a range of indices.  This list
      -- contains the lower bounds of these ranges.
    (lastWord, lastWordStartingIndex) =
      last $ zip possibleLastWords $ takeWhile (<=idx) startingIndexGivenLastWord
    lastWordLen = length $ stressPattern lastWord
    n' = n - lastWordLen
    idx' = idx - lastWordStartingIndex
