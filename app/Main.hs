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

-- The number of random bytes to be generated with paranoidRandomBytes.
kMaxRandomBytes = 1000000

-- A paranoid way of generating random bits (we use the 'entropy' module, the 'HsOpenSSL' module and
-- the 'cryptonite' module and xor the results together.)
paranoidRandomBytes :: Int -> IO RandomBytes
paranoidRandomBytes n = do
  e1 <- unpack <$> SE.getEntropy n
  e2 <- unpack <$> OpenSSLRand.randBytes n
  e3 <- unpack <$> CRE.getEntropy n
  let xor3 a b c = xor (xor a b) c
  return $ RandomBytes $ zipWith3 xor3 e1 e2 e3

-- A wrapper which exposes some random bytes we've pre-generated as a RandomGen instance.
newtype RandomBytes = RandomBytes [Word8]

instance RandomGen RandomBytes where
  next (RandomBytes (x:xs)) = (fromIntegral x, RandomBytes xs)
  next (RandomBytes []) = error "Random bytes exhausted.  Try a higher value of kMaxRandomBytes."
  genRange _ = (0, 255)
  split _ = error "Split is not implemented for RandomBytes."


----- Main program

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
sampleAndPrintRhymes :: (RandomGen g) => [Pronunciation] -> [Bool] -> Int -> Int -> g -> IO g
sampleAndPrintRhymes dictionary lineMeter k n g = do
  -- First we pick the rhyming words:
  let endWords = [p | p <- dictionary, stressPattern p `suffixOf` lineMeter]
    -- Words that match our meter if they're at the end of the line.
  let rhymePairs = pairsFromSets (rhymeSets endWords)
  let ((w1, w2), g2) = choose rhymePairs g
  -- Then we generate the rest of the couplet
  let line1Remainder = take (length lineMeter - length (stressPattern w1)) lineMeter
  let line2Remainder = take (length lineMeter - length (stressPattern w2)) lineMeter
  let line1Possibilities = countLines dictionary line1Remainder
  let line2Possibilities = countLines dictionary line2Remainder
  let (line1Index, g3) = randomR (0, line1Possibilities-1) g2
  let (line2Index, g4) = randomR (0, line2Possibilities-1) g3

  let line1 = constructLine dictionary line1Remainder line1Index ++ [w1]
  let line2 = constructLine dictionary line2Remainder line2Index ++ [w2]

  -- Show the passrhyme:
  putStrLn $ ""
  putStrLn $ "Sample #" ++ show k ++ ":"
  putStrLn $ showLine line1
  putStrLn $ showLine line2

  -- And print out some surprise stats:
  let rhymeBits = bits (length rhymePairs)
  let restBits = bits line1Possibilities + bits line2Possibilities
  let totalBits = rhymeBits + restBits
  putStrLn $ ""
  putStrLn $ "Selecting the rhyme gave us " ++ show rhymeBits ++ " bits of surprise."
  putStrLn $ "Given those rhyme words, the remainder had " ++ show restBits ++ " bits of surprise."
  putStrLn $ "For a total surprise of: " ++ show totalBits ++ " bits."
  putStrLn $ ""

  -- And maybe do it again.
  g5 <- if (k >= n)
    then return g4
    else sampleAndPrintRhymes dictionary lineMeter (k+1) n g4

  return g5

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
