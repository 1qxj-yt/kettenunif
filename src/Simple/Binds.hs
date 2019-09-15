module Simple.Binds
    ( Multiset
    -- * Construction
    , fromList
    , cons
    -- * Tranformation
    , deleteAt
    , diff
    , disjoint
    , partitions
    , partitionsWithRest
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
import qualified Data.IntMap as IM
import Data.Monoid(All(All),getAll)
import Data.List(nubBy)
import Data.Maybe(fromJust)

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


data Args a b = A { msSeqM :: Seq.Seq b, dmsM :: DMS.MultiSet b
                  , dmsE :: DMS.MultiSet a}

partitions :: (Show a,Show b,Ord a, Ord b) => Multiset b -> Multiset a -> [Int -> Multiset a]
partitions m e = partitionsRec (A (ripMS m) (toDMS m) (toDMS e)) IS.empty e
    where
        ripMS (MS m) = m
        toDMS (MS l) = DMS.fromList (toList l)
        partitionsRec :: (Show a,Show b,Ord a,Ord b) =>
            Args a b -> IS.IntSet -> Multiset a -> [Int -> Multiset a]
        partitionsRec _ _ e@(MS Seq.Empty)  = [const e]
        partitionsRec args u (MS e) = concat [
                            let (eL,eR) = Seq.splitAt i e
                                Just targetVar = msSeqM args Seq.!? (target-1)
                                tV_occurence = DMS.occur targetVar (dmsM args)
                                exprRepeated = Seq.cycleTaking (length eR * tV_occurence) eR
                                notDiscard = toDMS (MS exprRepeated) `DMS.isSubsetOf` dmsE args
                            in  if notDiscard then
                                    map (\f n -> if n==target then MS eR else f n)
                                        (partitionsRec args (IS.insert target u) (MS eL))
                                else {-traceShow (exprRepeated,e)-} []
                        | i<-[0..(length e-1)], target<-[1..(length m)], target `IS.notMember` u ]

partitionsWithRest :: (Show a,Show b,Ord a, Ord b) => Multiset b -> Multiset a -> [(b -> Multiset a, Multiset a)]
partitionsWithRest ms es = do
        let dmsM = toDMS ms
        let ms'  = nub' ms
        let fullToNub = let MS s = ms'
                            in IM.fromList $ Seq.foldMapWithIndex (\i (ix,_) -> [(ix,i+1)]) s
        χ <- partitions ms' es
        --let args = [(m',DMS.occur m' dmsM, χ(fromJust $ IM.lookup ix fullToNub) ) | (ix,m') <- toList ms']
        (dmsF,dmsR) <- applyRepPtWR $ do
            (ix,m') <- toList ms'
            return ( m',DMS.occur m' dmsM, χ(fromJust $ IM.lookup ix fullToNub) )
        return (fromList . toList . dmsF, fromList $ DMS.toList dmsR)
    where
        toOccList (MS m) = DMS.toOccurList . DMS.fromList $ toList m
        toDMS (MS m) = DMS.fromList $ toList m
        nub' :: Eq c => Multiset c -> Multiset (Int,c)
        nub' =  fromList . nubBy (\(_,l) (_,r) -> l==r) . zip [1..] . toList

applyRepPtWR :: (Ord a, Eq b) => [(b,Int,Multiset a)] -> [(b -> DMS.MultiSet a,DMS.MultiSet a)]
applyRepPtWR [] = [(const DMS.empty, DMS.empty)]
applyRepPtWR ((m,occ,e):xs) = [
            (\n -> if m == n then result else f n, r `DMS.union` rest)
         | (f,r) <- applyRepPtWR xs, (result,rest) <- repPartWR occ (toOccList e) ]
    where
        toOccList (MS m) = DMS.toOccurList . DMS.fromList $ toList m

-- Repetitive partition with rest.
repPartWR :: Ord a => Int -> [(a,Int)] -> [(DMS.MultiSet a,DMS.MultiSet a)]
repPartWR n [] = [(DMS.empty, DMS.empty)]
repPartWR n ((a,oc):xs) =
        let divCeil n m = let (d,r) = divMod n m in if r > 0 then d+1 else d
            minOcc = oc `divCeil` n
        in do
            oc'<-[minOcc..oc]
            let newXS = if oc' > 0 then DMS.fromOccurList [(a,oc')] else DMS.empty
            (xs,r) <- repPartWR n xs
            let newR  = if oc'*n > oc then DMS.fromOccurList [(a,oc'*n - oc)] else DMS.empty
            return (xs `DMS.union` newXS, r `DMS.union` newR)
