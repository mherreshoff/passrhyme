module Main where

import System.Random
import Lib
import Data.List (intercalate)
import qualified Data.Set as Set


-- How to print a line (list of words) to the screen.
showLine :: [Pronunciation] -> String
showLine ps = intercalate " " (map word ps)

-- The default meter used by readStressPattern:
iambicTetrameter = take 8 (cycle [False, True])

-- How to ask the user for a string of dots and dashes to structure our passrhyme.
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
  allowedWords <- (Set.fromList . lines) <$> readFile "google-10000-english-no-swears.txt"
  let dictionary = filter ((`Set.member`allowedWords) . word) largeDictionary
    -- Keep only the allowed words.  (CMU Dict has lots of weird proper names, etc.)
  -- putStrLn $ "numWords=" ++ show (length dictionary)
  lineMeter <- readStressPattern
  g <- getStdGen
    -- Get the system tandom numbers generator.
    -- TODO: replace with something more secure since we're using this as a password generator?
  -- First we pick the rhyming words:
  let endWords = [p | p <- dictionary, stressPattern p `suffixOf` lineMeter]
    -- Words that match our meter if they're at the end of the line.
  let ((w1, w2), g2) = chooseRandomPair (rhymeSets endWords) g
  -- Then we generate the two lines of poetry
  let (line1, g3) = generateLineWithLastWord dictionary lineMeter w1 g2
  let (line2, _) = generateLineWithLastWord dictionary lineMeter w2 g3
  -- Finally, show them:
  putStrLn $ showLine line1
  putStrLn $ showLine line2
