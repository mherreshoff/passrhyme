{-# LANGUAGE RankNTypes #-}
module Lib
  ( prefixOf
  , suffixOf
  , pairsFromSets 
  , choose
  , chooseRandomPair 
  , Pronunciation(Pronunciation)
  , word, phonemes
  , readDictionaryFile
  , parseDictionaryFile
  , nonAlternate
  , stressPattern
  , tailsWhere
  , rhymeStem
  , rhymeSets
  , generateLine
  ) where

import Control.Arrow
import qualified Data.Map as Map
import Data.Char (toLower, isNumber)
import Data.Map (Map)
import Data.List (tails)
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

------ Random Utils:

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

-- Weeds out alternate pronunciations (eg. "foo(1)").
nonAlternate :: Pronunciation -> Bool
nonAlternate p = last (word p) /= ')'

parseDictionaryFile :: String -> [Pronunciation]
parseDictionaryFile x = map parseEntry $ filter notComment $ lines x where
  notComment s = not (null s) && head s /= ';'
  parseEntry line = Pronunciation (map toLower word) phonemes where
    (word:phonemes) = words line

stressPattern :: Pronunciation -> [Bool]
stressPattern p = pattern where
  pattern = map (/='0') $ filter isNumber $ map last $ phonemes p

tailsWhere :: (a -> Bool) -> [a] -> [[a]]
tailsWhere pred = iter where
  iter [] = []
  iter t@(x:xs) = (if pred x then (t:) else id) $ iter xs

isStressedVowel :: String -> Bool
isStressedVowel s = c == '1' || c == '2' where c = last s

rhymeStem :: Pronunciation -> [String]
rhymeStem = last . tailsWhere isStressedVowel . phonemes

rhymeSets :: [Pronunciation] -> [[Pronunciation]]
rhymeSets = Map.elems . Map.fromListWith (++) . map (rhymeStem &&& (\x -> [x]))

generateLine :: (RandomGen g) => [Pronunciation] -> [Bool] -> g -> ([Pronunciation], g)
generateLine dictionary = iter where
  iter [] g = ([], g)
  iter xs g = (w:ws, g'') where
   (w, g') = choose [p | p <- dictionary, stressPattern p `prefixOf` xs] g
   xs' = tails xs !! (length $ stressPattern w)
   (ws, g'') = iter xs' g'
