{-# LANGUAGE TypeFamilies #-}
module Choice
  ( Choice, countChoices, choose
  , listToChoice
  , choiceToList
  , choiceUnion
  , choiceProduct
  , choicePair
  ) where
import Data.List ( genericIndex, genericLength )

----- Choice Data-Structure:
data Choice a = Choice Integer (Integer -> a)
-- A 'Choice a' object represents a uniform distribution over a set of 'a's.
-- It knows how many there are and how to pick one given an index.
-- It is more efficient than a list because the possibilities need not all be
-- layed out in memory.


----- Accessors:
countChoices :: Choice a -> Integer
countChoices (Choice n _) = n

choose :: Choice a -> Integer -> a
choose (Choice _ c) idx = c idx


----- Instances:
instance Functor Choice where
  fmap :: (a -> b) -> Choice a -> Choice b
  fmap f (Choice n g) = Choice n (f . g)

instance Applicative Choice where
  pure x = Choice 1 (const x)
  cf <*> cx = (\(f, x) -> f x) <$> choiceProduct cf cx

instance Monad Choice where
  return = pure
  (>>=) :: Choice a -> (a -> Choice b) -> Choice b
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

-- chooses among *distinct* pairs
choicePair :: Choice a -> Choice (a, a)
choicePair x = Choice (c * c - 1) locate where
  c = countChoices x
  locate idx = (choose x idx1, choose x idx2) where
    (idx1, idx2') = divMod idx (c - 1)
    idx2 = if idx2' >= idx1 then idx2' + 1 else idx2'
