module Main where

import Data.List (intercalate)
import Data.Word
import qualified Data.Set as Set
import System.Random

import Choice
  ( chooseRandomly, countChoices, listToChoice, choicePair, choiceUnion )
import Lib
import Random


-- The number of random bytes to be generated with paranoidRandomBytes.
kMaxRandomBytes = 1000000

-- The default meter used by readStressPattern:
iambicTetrameter = take 8 (cycle [False, True])

-- How to ask the user for a string of dots and dashes to structure our passrhyme.
readStressPattern :: IO [Bool]
readStressPattern = do
  putStrLn "Enter a string of dots and dashes to select your meter."
  putStrLn "(- means a stressed syllable and . means an unstressed syllable.)"
  putStrLn "Press Enter (blank input) for iambic tetrameter (.-.-.-.-)"
  line <- getLine
  if any (\c -> c /='.' && c /= '-') line
  then do
    putStrLn "Parse error, try again."
    readStressPattern
  else if null line then return iambicTetrameter
  else return (map (=='-') line)

-- Computes the log base 2 of an integer.
bits :: (Integral n) => n -> Double
bits = logBase 2.0 . fromIntegral

-- How to print a line (list of words) to the screen.
showLine :: [Pronunciation] -> String
showLine ps = intercalate " " (map word ps)

-- Given a dictionary, a line meter (stress pattern) and a random number generator, sample some
-- passrhymes and print them (including entropy information.)
sampleAndPrintRhymes :: (Show g, RandomGen g) => [Pronunciation] -> [Bool] -> Int -> Int -> g -> IO g
sampleAndPrintRhymes dictionary lineMeter k n g = do
  let couplets = coupletChoice dictionary lineMeter

  let ((line1, line2), g') = chooseRandomly couplets g

  -- Show the passrhyme:
  putStrLn $ ""
  putStrLn $ "Sample #" ++ show k ++ ":"
  putStrLn $ showLine line1
  putStrLn $ showLine line2

  let totalBits = bits (countChoices couplets)
  putStrLn $ ""
  putStrLn $ "Total surprise: " ++ show totalBits ++ " bits."
  putStrLn $ ""

  -- And maybe do it again.
  g'' <- if (k >= n)
    then return g'
    else sampleAndPrintRhymes dictionary lineMeter (k+1) n g'

  return g''

main :: IO ()
main = do 
  largeDictionary <- readDictionaryFile "cmudict-0.7b"
    -- Read the CMU Pronunciation Dictionary.
  allowedWords <- (Set.fromList . lines) <$> readFile "wordlist.txt"
    -- Read the list of allowed words.
  let dictionary' = filter ((`Set.member`allowedWords) . word) largeDictionary
    -- Keep only the allowed words.  (CMU Dict has lots of weird proper names, etc.)
  let dictionary = filter ((>1).length.stressPattern) dictionary'
    -- Throw out the one syllable words (since we're using a uniform sampler, we'd end up with many
    -- passphrases made mostly of one syllable words otherwise, and those are harder to memorize.)
  lineMeter <- readStressPattern
    -- Ask the user for a stress pattern.
  g <- paranoidRandomBytes kMaxRandomBytes
    -- Generate a random byte-string to sample passrhymes.
  _ <- sampleAndPrintRhymes dictionary lineMeter 1 5 g
    -- Print five examples.
  return ()
