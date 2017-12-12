module Main where

import System.Random
import Lib
import Data.List (intercalate, inits)
import qualified Data.Set as Set

-- Meter is currently iambic Tetrameter.  Alter this line to change the meter of your password.
lineMeter = take 8 (cycle [False, True])

completeLine :: (RandomGen g)
    => [Pronunciation] -> Pronunciation -> g -> ([Pronunciation], g)
completeLine dictionary rhymeWord g = (lineInit ++ [rhymeWord], g') where
  rhymeWordLength = length (stressPattern rhymeWord)
  meter = inits lineMeter !! (length lineMeter - rhymeWordLength)
  (lineInit, g') = generateLine dictionary meter g

showLine :: [Pronunciation] -> String
showLine ps = intercalate " " (map word ps)

main :: IO ()
main = do 
  largeDictionary <- readDictionaryFile "cmudict-0.7b"
  diceware <- (Set.fromList . lines) <$> readFile "diceware.txt"
  let dictionary = filter ((`Set.member`diceware) . word) largeDictionary
  -- putStrLn $ "d=" ++ show (length dictionary)
  g <- getStdGen
  -- First we pick the rhyming words:
  let endWords = [p | p <- dictionary, stressPattern p `suffixOf` lineMeter]
  let ((w1, w2), g2) = chooseRandomPair (rhymeSets endWords) g
  -- Then we complete the two lines of poetry
  let (line1, g3) = completeLine dictionary w1 g2
  let (line2, _) = completeLine dictionary w2 g3
  -- Finally, show them:
  putStrLn $ showLine line1
  putStrLn $ showLine line2
