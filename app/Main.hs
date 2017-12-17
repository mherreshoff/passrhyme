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

bits :: (Integral n) => n -> Double
bits = logBase 2.0 . fromIntegral

-- Given a dictionary, a line meter (stress pattern) and a random number generator, sample a passrhyme and print it (including
-- entropy information.)
sampleAndPrintRhyme :: (RandomGen g) => [Pronunciation] -> [Bool] -> g -> IO g
sampleAndPrintRhyme dictionary lineMeter g = do
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

  -- Then, show them:
  putStrLn $ showLine line1
  putStrLn $ showLine line2

  -- And print out some stats:
  let rhymeBits = bits (length rhymePairs)
  let restBits = bits line1Possibilities + bits line2Possibilities
  let totalBits = rhymeBits + restBits
  putStrLn $ ""
  putStrLn $ "Selecting the rhyme gave us " ++ show rhymeBits ++ " bits of surprize."
  putStrLn $ "For those rhyme words, we had " ++ show restBits ++ " bits of surprize."
  putStrLn $ "For a total surprize of: " ++ show totalBits ++ " bits."
  return g4

main :: IO ()
main = do 
  largeDictionary <- readDictionaryFile "cmudict-0.7b"
  allowedWords <- (Set.fromList . lines) <$> readFile "google-10000-english-no-swears.txt"
  let dictionary' = filter ((`Set.member`allowedWords) . word) largeDictionary
  let dictionary = filter ((>1).length.stressPattern) dictionary'
    -- Keep only the allowed words.  (CMU Dict has lots of weird proper names, etc.)
  -- putStrLn $ "numWords=" ++ show (length dictionary)
  lineMeter <- readStressPattern
  g <- paranoidRandomBytes kMaxRandomBytes
  _ <- sampleAndPrintRhyme dictionary lineMeter g
  return ()
