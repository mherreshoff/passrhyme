module Main where

import qualified Crypto.Random.Entropy as CRE
import Data.Bits (xor)
import Data.ByteString (unpack)
import Data.List (intercalate)
import Data.Word
import Lib
import qualified OpenSSL.Random as OpenSSLRand
import qualified System.Entropy as SE
import System.Random
import qualified Data.Set as Set


----- Randomness utils:

-- A paranoid way of generating random bits (we use the 'entropy' module, the 'HsOpenSSL' module and the cryptonite module and
-- xor the results together.)
paranoidRandomBytes :: Int -> IO RandomBytes
paranoidRandomBytes n = do
  e1 <- unpack <$> SE.getEntropy n
  e2 <- unpack <$> OpenSSLRand.randBytes n
  e3 <- unpack <$> CRE.getEntropy n
  let xor3 a b c = xor (xor a b) c
  return $ RandomBytes $ zipWith3 xor3 e1 e2 e3


-- An interface to expose some random bytes we've gotten from the system as a RandomGen object.
newtype RandomBytes = RandomBytes [Word8]

kMaxRandomBytes = 1000000

instance RandomGen RandomBytes where
  next (RandomBytes (x:xs)) = (fromIntegral x, RandomBytes xs)
  next (RandomBytes []) = error "Random bytes exhausted.  Try a higher value of kMaxRandomBytes."
  genRange _ = (0, 255)
  split _ = error "Not implemented."

----- Main program
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
  g <- paranoidRandomBytes kMaxRandomBytes
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

  let l2 = map (logBase 2.0 . fromIntegral)
  putStrLn $ "itermediate bit counts:" ++ show (l2 $ countLinesWithIntermediates dictionary lineMeter)
