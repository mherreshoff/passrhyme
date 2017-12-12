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
  , generateLine
  , generateLineWithLastWord
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

tailsWhere :: (a -> Bool) -> [a] -> [[a]]
tailsWhere pred = iter where
  iter [] = []
  iter t@(x:xs) = (if pred x then (t:) else id) $ iter xs


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
-- a stressed syllable and False indicating an unstressed syllabel.
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

-- Groups Pronunciations by their 'rymeStem's.
rhymeSets :: [Pronunciation] -> [[Pronunciation]]
rhymeSets = Map.elems . Map.fromListWith (++) . map (rhymeStem &&& (\x -> [x]))

-- Given a Pronunciation list for all the available words and a meter, produce a line of poetry.
generateLine :: (RandomGen g) => [Pronunciation] -> [Bool] -> g -> ([Pronunciation], g)
generateLine dictionary = iter where
  iter [] g = ([], g)
  iter xs g = (w:ws, g'') where
   (w, g') = choose [p | p <- dictionary, stressPattern p `prefixOf` xs] g
   xs' = tails xs !! (length $ stressPattern w)
   (ws, g'') = iter xs' g'

-- The same with a fixed last word (used to create rhyming couplets.)
generateLineWithLastWord :: (RandomGen g)
    => [Pronunciation] -> [Bool] -> Pronunciation -> g -> ([Pronunciation], g)
generateLineWithLastWord dictionary lineMeter rhymeWord g = (lineInit ++ [rhymeWord], g') where
  rhymeWordLength = length (stressPattern rhymeWord)
  meter = inits lineMeter !! (length lineMeter - rhymeWordLength)
  (lineInit, g') = generateLine dictionary meter g
