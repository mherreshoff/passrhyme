module Main where

import System.Random
import Lib
import Data.List (intercalate, inits)
import qualified Data.Set as Set

iambicTetrameter = take 8 (cycle [False, True])

-- Generates a line given a rhymeword.
completeLine :: (RandomGen g)
    => [Pronunciation] -> [Bool] -> Pronunciation -> g -> ([Pronunciation], g)
completeLine dictionary lineMeter rhymeWord g = (lineInit ++ [rhymeWord], g') where
  rhymeWordLength = length (stressPattern rhymeWord)
  meter = inits lineMeter !! (length lineMeter - rhymeWordLength)
  (lineInit, g') = generateLine dictionary meter g

showLine :: [Pronunciation] -> String
showLine ps = intercalate " " (map word ps)

readStressPattern :: IO [Bool]
readStressPattern = do
  putStrLn "Enter a string of dots and dashes to select your meter."
  putStrLn "(- means a stressed sylable and . means an unstressed sylable.)"
  putStrLn "Press Enter (blank input) for iambic tetrameter (.-.-.-.-)"
  line <- getLine
  if any (\c -> c /='.' && c /= '-') line
  then do
    putStrLn "Parse error, try again."
    readStressPattern
  else if null line then return iambicTetrameter
  else return (map (=='-') line)

main :: IO ()
main = do 
  largeDictionary <- readDictionaryFile "cmudict-0.7b"
  diceware <- (Set.fromList . lines) <$> readFile "google-10000-english-no-swears.txt"
  let dictionary = filter ((`Set.member`diceware) . word) largeDictionary
  lineMeter <- readStressPattern
  -- putStrLn $ "d=" ++ show (length dictionary)
  g <- getStdGen
  -- First we pick the rhyming words:
  let endWords = [p | p <- dictionary, stressPattern p `suffixOf` lineMeter]
  let ((w1, w2), g2) = chooseRandomPair (rhymeSets endWords) g
  -- Then we complete the two lines of poetry
  let (line1, g3) = completeLine dictionary lineMeter w1 g2
  let (line2, _) = completeLine dictionary lineMeter w2 g3
  -- Finally, show them:
  putStrLn $ showLine line1
  putStrLn $ showLine line2
