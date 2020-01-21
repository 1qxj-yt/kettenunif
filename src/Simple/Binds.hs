module Simple.Binds
    ( Multiset
    -- * Construction
    , fromList
    , cons
    -- * Tranformation
    , deleteAt
    , diff
    , disjoint
    , toOccList
    , dPart
    -- * Decomposition
    , uncons
    , Simple.Binds.head
    , Simple.Binds.tail
    -- * Folds
    , foldMapWithIndex
    ) where

import qualified Data.Sequence as Seq
import Data.Foldable(toList)

import qualified Data.MultiSet as DMS
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Monoid(All(All),getAll)
import Data.List(nubBy)
import Control.Arrow((***))

newtype Multiset a = MS (Seq.Seq a)


------------------------------------------------
-- Instances
------------------------------------------------

instance (Eq a, Ord a) => Eq (Multiset a) where
    MS e1 == MS e2 = Seq.unstableSort e1 == Seq.unstableSort e2

instance Ord a => Ord (Multiset a) where
    compare (MS e1) (MS e2) = compare (length e1) (length e2) `mappend` compare e1 e2

instance Show a => Show (Multiset a) where
    show (MS e) = drop (Prelude.length "fromList ") (show e)

instance Monoid (Multiset a) where
    mempty = MS Seq.empty
    mappend (MS a) (MS b) = MS (a Seq.>< b)

instance Functor Multiset where
    fmap f (MS e) = MS (fmap f e)

instance Foldable Multiset where
    foldMap f (MS e) = foldMap f e

------------------------------------------------
-- Functions
------------------------------------------------

fromList :: [a] -> Multiset a
fromList = MS . Seq.fromList

deleteAt :: Int -> Multiset a -> Multiset a
deleteAt i (MS s) = MS (Seq.deleteAt i s)

cons :: a -> Multiset a -> Multiset a
cons b (MS e) = MS (b Seq.<| e)

uncons :: Multiset a -> (a, Multiset a)
uncons (MS (b Seq.:<| bs)) = (b, MS bs)

head :: Multiset a -> a
head (MS (b Seq.:<|  _)) = b

tail :: Multiset a -> Multiset a
tail (MS (_ Seq.:<| bs)) = MS bs

foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Multiset a -> m
foldMapWithIndex f (MS s) = Seq.foldMapWithIndex f s

diff :: (Eq a,Ord a) => Multiset a -> Multiset a -> Multiset a
diff (MS e1) (MS e2) = fromList $ toList $ make e1 DMS.\\ make e2
    where make = DMS.fromList . toList

disjoint :: Ord a => Multiset a -> Multiset a -> Bool
disjoint e1 e2 = let (MS l,MS r) = if length e1 < length e2 then (e1,e2) else (e2,e1)
    in getAll $ foldMap (All . (`S.notMember` (S.fromList $ toList r))) (S.fromList $ toList l)


------------------------------------------------
-- Partitions
------------------------------------------------

-- | Distinct partition.
-- dPart m e partitions e onto m, where m is assumed to be distinct.
dPart :: (Show a, Show b, Eq b) => Multiset b -> Multiset a -> [b -> Multiset a]
dPart (MS ms) (MS Seq.Empty) = [mempty]
dPart (MS ms) (MS (b Seq.:<| es)) = do
        f <- dPart (MS ms) (MS es)
        m <- toList ms
        let MS res = f m
        return $ \var -> if var==m then MS (b Seq.<| res) else f var

-- ### Transformations ### --

toOccList :: Ord a => Multiset a -> [(a, DMS.Occur)]
toOccList (MS m) = DMS.toOccurList . DMS.fromList $ toList m
