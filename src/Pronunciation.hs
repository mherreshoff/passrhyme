module Pronunciation
  ( Pronunciation(Pronunciation)
  , word, phonemes
  , readDictionaryFile
  , parseDictionaryFile
  , nonAlternate
  , stressPattern
  , rhymeStem
  ) where
import Data.Char (toLower, isNumber)
import Data.List (tails)

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
 t = filter startsWithStress $ tails $ phonemes p
 startsWithStress [] = False
 startsWithStress (x:_) = isStressedVowel x

