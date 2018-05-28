module Random
  ( RandomBytes
  , paranoidRandomBytes
  ) where
import Data.Bits (xor)
import Data.ByteString (unpack)
import Data.Word
import qualified Crypto.Random.Entropy as CRE
import qualified OpenSSL.Random as OpenSSLRand
import qualified System.Entropy as SE
import System.Random (RandomGen(next, genRange, split))

----- Randomness utils:

-- A wrapper which exposes some random bytes we've pre-generated as a RandomGen instance.
newtype RandomBytes = RandomBytes [Word8]

instance RandomGen RandomBytes where
  next (RandomBytes (x:xs)) = (fromIntegral x, RandomBytes xs)
  next (RandomBytes []) = error "Random bytes exhausted."
  genRange _ = (0, 255)
  split _ = error "Split is not implemented for RandomBytes."

-- A paranoid way of generating random bits (we use the 'entropy' module, the 'HsOpenSSL' module and
-- the 'cryptonite' module and xor the results together.)
paranoidRandomBytes :: Int -> IO RandomBytes
paranoidRandomBytes n = do
  e1 <- unpack <$> SE.getEntropy n
  e2 <- unpack <$> OpenSSLRand.randBytes n
  e3 <- unpack <$> CRE.getEntropy n
  let xor3 a b c = xor (xor a b) c
  return $ RandomBytes $ zipWith3 xor3 e1 e2 e3

