{-# LANGUAGE RankNTypes #-}
module Lib
  ( prefixOf
  , suffixOf
  , tailsWhere
  , Pronunciation(Pronunciation)
  , word, phonemes
  , readDictionaryFile
  , parseDictionaryFile
  , nonAlternate
  , stressPattern
  , rhymeStem
  , rhymeSets
  , lineChoice
  , lineChoiceSeq
  ) where

import Control.Arrow
import qualified Data.Map as Map
import Data.Char (toLower, isNumber)
import Data.Map (Map)
import Data.List (inits, tails)
import System.Random

import Choice

----- Generic Utility Code:

-- Checks whether the second list starts with the first list.
prefixOf :: (Eq a) => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

-- Checks whether the second list ends with the first list.
suffixOf :: (Eq a) => [a] -> [a] -> Bool
suffixOf xs ys = prefixOf (reverse xs) (reverse ys)

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

lineChoice :: [Pronunciation] -> [Bool] -> Choice [Pronunciation]
lineChoice dictionary pattern = last $ lineChoiceSeq dictionary pattern

lineChoiceSeq :: [Pronunciation] -> [Bool] -> [Choice [Pronunciation]]
lineChoiceSeq dictionary pattern = resultList where
  wordChoiceByStressMap = listToChoice <$> keyBy stressPattern dictionary
  wordChoice :: [Bool] -> Choice Pronunciation
  wordChoice sp = maybe mempty id (Map.lookup sp wordChoiceByStressMap)
  transitions n = nonEmptyTails $ take n pattern
  resultList = fmap result [0..length pattern]
  result :: Int -> Choice [Pronunciation]
  result 0 = listToChoice [[]]
  result n = mconcat
    [(\x y -> x ++ [y]) <$> (resultList !! (n - length t)) <*> wordChoice t
      | t <- transitions n]
