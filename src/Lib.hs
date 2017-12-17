{-# LANGUAGE RankNTypes #-}
module Lib
  ( prefixOf
  , suffixOf
  , pairsFromSets 
  , tailsWhere
  , choose
  , chooseRandomPair 
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

----- List Utils

prefixOf :: (Eq a) => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

suffixOf :: (Eq a) => [a] -> [a] -> Bool
suffixOf xs ys = prefixOf (reverse xs) (reverse ys)

pairsFromSets :: forall a . [[a]] -> [(a,a)]
pairsFromSets = concatMap pairs where
  pairs :: [a] -> [(a,a)]
  pairs [] = []
  pairs (x:xs) = map (\x' -> (x, x')) xs ++ pairs xs

nonEmptyTails :: [a] -> [[a]]
nonEmptyTails = tailsWhere (const True)

tailsWhere :: (a -> Bool) -> [a] -> [[a]]
tailsWhere pred = iter where
  iter [] = []
  iter t@(x:xs) = (if pred x then (t:) else id) $ iter xs

keyBy :: (Ord k) => (v -> k) -> [v] -> Map k [v]
keyBy f vs = Map.fromListWith (++) $ map (f &&& (\v -> [v])) vs

------ Random Utils:

-- Pick a random element from a list.
choose :: (RandomGen g) => [a] -> g -> (a, g)
choose xs g = (x, g') where
  (idx, g') = randomR (0, length xs - 1) g
  x = xs !! idx

chooseRandomPair :: (RandomGen g) => [[a]] -> g -> ((a,a), g)
chooseRandomPair sets g = choose (pairsFromSets sets) g

----- CMU Dictionary Utils

-- An entry from the CMU pronunciation dictionary.
data Pronunciation = Pronunciation
  { word :: String
  , phonemes :: [String] }
  deriving (Eq, Ord, Show)

readDictionaryFile :: FilePath -> IO [Pronunciation]
readDictionaryFile f = do 
  s <- readFile f
  return $ parseDictionaryFile s

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
rhymeSets = Map.elems . Map.fromListWith (++) . map (rhymeStem &&& (\x -> [x]))

-- countLines
countLines :: [Pronunciation] -> [Bool] -> Integer
countLines dictionary pattern = last $ countLinesWithIntermediates dictionary pattern

countLinesWithIntermediates :: [Pronunciation] -> [Bool] -> [Integer]
countLinesWithIntermediates dictionary pattern = resList where
  dictCountsByStress = fromIntegral.length <$> keyBy stressPattern dictionary
  count sp = maybe 0 id (Map.lookup sp dictCountsByStress)
  resList :: [Integer]
  resList = [1] ++ [res n | n <- [1..length pattern]]
  transitions n = nonEmptyTails $ take n pattern
  res n = sum [resList !! (n-length t) * count t | t <- transitions n]

constructLine :: [Pronunciation] -> [Bool] -> Integer -> [Pronunciation]
constructLine dictionary pattern index = result where
  wordsByStressMap = keyBy stressPattern dictionary
  wordsByStress sp = maybe [] id (Map.lookup sp wordsByStressMap)
  counts = countLinesWithIntermediates dictionary pattern
  result = iter (fromIntegral $ length pattern) index
  transitions n = nonEmptyTails $ take n pattern
  iter :: Int -> Integer -> [Pronunciation]
  iter 0 _ = []
  iter n idx = iter n' idx' ++ [lastWord] where
    possibleLastWords = concatMap wordsByStress $ transitions n
    numPossibilitiesGivenLastWord =
      [counts !! (n - length (stressPattern p)) | p <- possibleLastWords]
    startingIndexGivenForLastWord = scanl (+) 0 numPossibilitiesGivenLastWord 
    lastWordId =
      fromIntegral $ subtract 1 $ length $ takeWhile (<=idx) startingIndexGivenForLastWord
    lastWord = possibleLastWords !! lastWordId
    lastWordLen = length $ stressPattern lastWord
    n' = n - lastWordLen
    idx' = idx - startingIndexGivenForLastWord !! lastWordId
