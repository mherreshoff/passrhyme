module Main where

import Data.List (intercalate)
import Data.Word
import qualified Data.Set as Set
import System.Random

import Choice
  ( Choice
  , chooseRandomly
  , choiceBits )
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

-- How to print a line (list of words) to the screen.
showLine :: [Pronunciation] -> String
showLine ps = intercalate " " (map word ps)

showCouplet :: ([Pronunciation], [Pronunciation]) -> String
showCouplet (l1,l2) = showLine l1 ++ "\n" ++ showLine l2


-- Given a Choice String, draw some samples and print them.
sampleAndPrint :: (Show g, RandomGen g) => Choice String -> Int -> Int -> g -> IO g
sampleAndPrint c k n g = do
  let (s, g') = chooseRandomly c g

  putStrLn $ ""
  putStrLn $ "Sample #" ++ show k ++ ":"
  putStrLn $ s

  -- And maybe do it again.
  g'' <- if (k >= n)
    then return g'
    else sampleAndPrint c (k+1) n g'

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

  let couplets = showCouplet <$> coupletChoice dictionary lineMeter
    -- Build a Choice object over all the allowed rhyming couplets.

  g <- paranoidRandomBytes kMaxRandomBytes
    -- Generate a random byte-string to sample passrhymes.

  _ <- sampleAndPrint couplets 1 5 g
    -- Print five examples.

  putStrLn $ ""
  putStrLn $ "Total entropy: " ++ show (choiceBits couplets) ++ " bits."
  putStrLn $ ""
    -- Print out the entropy so the user knows the password strength.

  return ()
