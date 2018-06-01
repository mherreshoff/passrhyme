{-# LANGUAGE TypeFamilies #-}
module Choice
  ( Choice, countChoices, choiceBits, choose
  , chooseRandomly
  , listToChoice
  , choiceToList
  , choiceUnion
  , choiceProduct
  ) where
import Data.List ( genericIndex, genericLength )
import System.Random

----- Choice Data-Structure:
data Choice a = Choice Integer (Integer -> a)
-- A 'Choice a' object represents an indexed collection of 'a's.
--
-- It knows how many there are and how to pick one given an index.
--
-- We use this data-structure instead of, say, a list because, by
-- using mechanisms like `choiceProduct`, we can create Choice
-- objects which represent lists far larger than could fit in memory.


----- Accessors:
countChoices :: Choice a -> Integer
countChoices (Choice n _) = n

choose :: Choice a -> Integer -> a
choose (Choice _ c) idx = c idx


------ Probabilistic Interpretation:
-- We can interpret a (non-empty) choice object as a uniform
-- distribution over its elements.

choiceBits :: Choice a -> Double
choiceBits = logBase 2.0 . fromIntegral . countChoices
-- Measures (in bits) the entropy of a choice (interpreted as a uniform distribution.)

chooseRandomly :: (Show g, RandomGen g) => Choice a -> g -> (a, g)
chooseRandomly c g = (choose c idx, g') where
  (idx, g') = randomR (0, countChoices c - 1) g where
-- Samples from a choice (interpreted as a uniform distribution.)


----- Instances:
instance Monoid (Choice a) where
  -- The Monoid instance unions choices together.  See `choiceUnion`.
  mempty = Choice 0 (\idx -> error "Attempted to choose from empty choice.")
  mappend a b = mconcat [a, b]
  mconcat = choiceUnion

instance Functor Choice where
  fmap f (Choice n g) = Choice n (f . g)

instance Applicative Choice where
  pure x = Choice 1 (const x)
  cf <*> cx = (\(f, x) -> f x) <$> choiceProduct cf cx

instance Monad Choice where
  return = pure
  ca >>= f = choiceUnion $ map f $ choiceToList ca


----- List Utils:
listToChoice :: [a] -> Choice a
listToChoice xs = Choice (genericLength xs) (genericIndex xs)

choiceToList :: Choice a -> [a]
choiceToList cx = map (choose cx) [0..(countChoices cx - 1)]


----- Combiners:
choiceUnion :: [Choice a] -> Choice a
choiceUnion xs = Choice (sum $ map countChoices xs) (locate xs) where
  locate [] idx = error "choiceUnion: index too large."
  locate (x:xs) idx = let c = countChoices x in
      if idx < c then choose x idx else locate xs (idx-c)

choiceProduct :: Choice a -> Choice b -> Choice (a, b)
choiceProduct x y = Choice (countChoices x * countChoices y) locate where
  locate idx = (choose x xi, choose y yi) where (xi, yi) = divMod idx (countChoices y)
