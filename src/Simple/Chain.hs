module Simple.Chain
    (
    -- * Check
    ladderDistinct
    -- * Retrieval
    , chainList
    -- * Result
    , chainExpr
    , asocs
    , varlist
    ) where

import Simple.Expression
    ( Expr
    , Binds
    , Bind((:=))
    , Var
    , SetVar(ChVar)
    , isMeta
    , from, to
    , var, meta
    )
import qualified Simple.Binds as B

import Data.Foldable(toList)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Control.Arrow((***),first,second)
import Data.List(permutations)
import Data.Maybe(fromMaybe)

import Debug.Trace

-- | A necessary condition for @bs@ to form a valid chain:
-- @bs@ contains no binding of the form a=a and all bindings are left- and right-distinct.
ladderDistinct :: Binds -> Bool
ladderDistinct bs = let blist = toList bs :: [Bind]
    in length bs == S.size (S.fromList $ map (\(a:=b) -> a) blist)
    && length bs == S.size (S.fromList $ map (\(a:=b) -> b) blist)
    && all (\(a:=b) -> a/=b) bs


-- | Converts a multiset of bindings into a list of "immediate" chains (molecules).
-- Assumes input to be ladder-distinct.
molecules :: Binds -> [Seq.Seq Var]
molecules bs = moleculesAux bs []
    where
    -- | For each molecule in @ac@, @moleculesAux bs ac@ tries to append the
    -- head of @bs@ to either the left of right hand side of the molecule.
    -- If this fails, a new molecule is added to @ac@.
    -- The proccess terminates when @bs@ is empty.
    moleculesAux :: Binds -> [Seq.Seq Var] -> [Seq.Seq Var]
    moleculesAux bs ac
        | null bs   = ac    -- base case (termination)
        | otherwise =  let  x:=y  = B.head bs
                            (nSeqL,updatedL) = maybeAppendLeft  x y ac
                            (nSeqR,updatedR) = maybeAppendRight x y ac
            in moleculesAux (B.tail bs) $
                    if updatedL then nSeqL       -- append to the beginning of molecule
                    else if updatedR then nSeqR  -- append to the end of the molecule
                    else Seq.fromList [x,y] : ac -- new molecule

    -- | Tries to append x=y to the left-hand side (beginning) of the molecule.
    -- Returns whether it succeeded.
    maybeAppendLeft :: Var -> Var -> [Seq.Seq Var] -> ([Seq.Seq Var], Bool)
    maybeAppendLeft x y [] = ([],False)
    maybeAppendLeft x y (s:ss) = if (let l Seq.:<| s' = s in y == l)
            then ((x Seq.<| s):ss, True)
            else first (s:) (maybeAppendLeft x y ss)

    -- | Tries to append x=y to the right-hand side (end) of the molecule.
    -- Returns whether it succeeded.
    maybeAppendRight :: Var -> Var -> [Seq.Seq Var] -> ([Seq.Seq Var], Bool)
    maybeAppendRight x y [] = ([],False)
    maybeAppendRight x y (s:ss) = if (let s' Seq.:|> r = s in x == r)
            then ((s Seq.|> y):ss, True)
            else first (s:) (maybeAppendRight x y ss)


data MolType =      LL -- X=…=a; left-hand side is meta
                |   RR -- a=…=X; right-hand side is meta
                |   BB -- X=…=Y; both sides are meta
                |   NN -- a=…=b; none of the sides are meta
                    deriving (Eq,Show)

-- | Returns the type of the molecule.
molType :: Seq.Seq Var -> MolType
molType sq
    | length sq < 2 = error "molecule shorter than two"
    | otherwise = let   (l Seq.:<| _) = sq
                        (_ Seq.:|> r) = sq
                in case (isMeta l, isMeta r) of
                    (True ,True ) -> BB
                    (True ,False) -> LL
                    (False,True ) -> RR
                    (False,False) -> NN


-- | States of the automaton.
data LeftOrRight  = L | R deriving (Eq,Show)
type ChainState = Maybe LeftOrRight

transitionTable :: ChainState -> MolType -> ChainState
transitionTable (Just L) trans
    | trans == LL           = Just L
    | trans == BB           = Just R
    | otherwise             = Nothing
transitionTable (Just R) trans
    | trans `elem` [RR,BB]  = Just R
    | otherwise             = Just L
transitionTable Nothing _   = Nothing

runMachine :: [MolType] -> ChainState
runMachine (c:cs) = let ini = Just $ if c `elem` [LL,NN] then L else R
    in foldl transitionTable ini cs


-- | Assumes @xs@ to be non-empty (and all molecules to be longer than two).
-- Return Nothing if c cannot be instantiated to xs.
-- Otherwise, it returns the list of new variable associations.
instantiable :: SetVar -> [Seq.Seq Var] -> Maybe [(Var,Var)]
instantiable c xs =
    let (a Seq.:<| _) = head xs
        (_ Seq.:|> b) = last xs
    in  case (from c == a, to c==b) of
            (True ,True ) -> Just []
            (False,True ) -> sel a (from c)
            (True ,False) -> sel b (to c)
            _ -> do
                s1 <- sel a (from c)
                s2 <- sel b (to c)
                return $ s1++s2
    where
    sel :: Var -> Var -> Maybe [(Var, Var)]
    sel x y
        | isMeta x && newInCh y = Just [(x,y)]
        | isMeta y && newInCh x = Just [(y,x)]
        | otherwise = Nothing

    stripHead :: [Seq.Seq Var] -> [Seq.Seq Var]
    stripHead ((_ Seq.:<| sq):rs) = sq:rs
    stripLast :: [Seq.Seq Var] -> [Seq.Seq Var]
    stripLast ss = let (rs Seq.:|> _) = last ss
                in init ss ++ [rs]
    newInCh :: Var -> Bool
    newInCh v = let inner = stripLast $ stripHead xs
                in  all (null . Seq.filter (== v)) inner


-- | Glue two molecules together into one.
singleBond :: Seq.Seq Var -> Seq.Seq Var -> (Seq.Seq Var, Maybe (Var,Var) )
singleBond (sq1 Seq.:|> s1) (s2 Seq.:<| sq2)
    | s1 == s2  = ( (sq1 Seq.:|> s1) Seq.>< sq2, Nothing )
    | isMeta s2 = ( (sq1 Seq.:|> s1) Seq.>< sq2, Just (s2,s1) )
    | isMeta s1 = ( (sq1 Seq.:|> s2) Seq.>< sq2, Just (s1,s2) )
    | otherwise  = error "Invalid bond"


-- | Glue a list of molecules together into one (by applying singleBond repeatedly).
bond :: [Seq.Seq Var] -> (Seq.Seq Var, [(Var,Var)])
bond []     = mempty
bond [t]    = (t,[])
bond (s:t:ss) = let (s', asoc) = singleBond s t
    in second (case asoc of Nothing -> id ; Just vv -> (vv:)) $
            bond (s':ss)

-- | Converts a list of molecules into bindings.
chain :: Seq.Seq Var -> [Bind]
chain Seq.Empty              = mempty
chain (t Seq.:<| Seq.Empty)  = mempty
chain (s Seq.:<| (t Seq.:<| ss)) = (s:=t) : chain (t Seq.:<| ss)




data ChainResult = CR {
                chainExpr :: Expr,
                asocs :: [(Var,Var)],
                varlist :: [Var]
            }
            deriving Show

chainList :: SetVar -> Binds -> [ChainResult]
chainList c bs = let    perm = permutations (molecules bs) -- permute all molecules
                        validC = filter ((/= Nothing) . runMachine . map molType) perm -- filter valid chains
                in do
                    ch <- validC
                    case instantiable c ch of
                        Nothing -> []
                        Just outerAsocs -> do
                            let (sq, innerAsocs) = bond ch
                                ((h Seq.:<| inn) Seq.:|> l) = sq
                                nh = fromMaybe h (lookup h outerAsocs)
                                nl = fromMaybe l (lookup l outerAsocs)
                                nsq = (nh Seq.:<| inn) Seq.:|> nl
                                resBinds = chain nsq
                            return $ CR (setExpr [] resBinds)
                                        (outerAsocs ++ innerAsocs)
                                        (toList nsq)
