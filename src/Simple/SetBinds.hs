module Simple.SetBinds
    ( Set
    -- * Construction
    , fromList
    , cons
    -- * Tranformation
    , deleteAt
    -- * Decomposition
    , uncons
    , Simple.SetBinds.head
    , Simple.SetBinds.tail
    -- * Folds
    , foldMapWithIndex
    ) where

import qualified Data.Set as S
import Data.List(nub)

newtype Set a = S [a] deriving (Ord)


------------------------------------------------
-- Instances
------------------------------------------------

instance (Eq a, Ord a) => Eq (Set a) where
    S e1 == S e2 = S.fromList e1 == S.fromList e2

instance Show a => Show (Set a) where
        show (S e) = show e

instance Eq a => Monoid (Set a) where
    mempty = S []
    mappend (S a) (S b) = S (nub $ a ++ b)

instance Functor Set where
    fmap f (S e) = S (map f e)

instance Foldable Set where
    foldMap f (S e) = foldMap f e

------------------------------------------------
-- Functions
------------------------------------------------

-- | Makes a list from a list which might contain duplicates.
fromList :: Eq a => [a] -> Set a
fromList = S . nub

-- | @deleteAt i e@ returns e with its (i+1)-th element deleted.
-- Returns original set if index is out of bounds.
deleteAt :: Int -> Set a -> Set a
deleteAt i (S s) = let (ini,iE:til) = splitAt i s
            in S (ini ++ til)

cons :: a -> Set a -> Set a
cons b (S e) = S (b:e)

uncons :: Set a -> (a, Set a)
uncons (S (b:bs)) = (b, S bs)

head :: Set a -> a
head e = fst (uncons e)

tail :: Set a -> Set a
tail e = snd (uncons e)

foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Set a -> m
foldMapWithIndex f (S s) = mconcat $ zipWith f [0..] s
