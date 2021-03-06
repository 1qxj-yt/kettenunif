module Simple.Expression
    ( Expr(SetExpr)
    , Binds
    , Bind((:=))
    , Var
    , SetVar(SetVar)
    , Token(B,V,E)
    -- * Decomposition
    , decompose
    -- * Expression
    , expr
    , setExpr
    , eConsS
    , eNull
    , eNullS
    , eLength
    , eLengthS
    , eHead
    , eHeadS
    , eTail
    , eDelete
    , eDeleteS
    , clean
    , disjointS
    , foldWithIndex
    , foldWithIndexSet
    , partition
    , isPartitionExpr
    -- * Variables
    , meta
    , var
    , isMeta
    , addApos
    , combine
    , sgSplit
    , prepareRec
    , stopRec
    , isCarrying
    , ωBind
    , ωExpr
    ) where

import Simple.Binds as B

import Data.Char(isUpper,isLower)
import Data.List(intercalate,delete)
import Data.Foldable(toList)
import Control.Arrow((***))
import qualified Data.Map.Strict as M

------------------------------------------------
-- Data
------------------------------------------------

data Expr = SetExpr SetVars Binds  deriving Eq
type SetVars = Multiset SetVar
type Binds = Multiset Bind

data Bind = Var := Var deriving (Eq,Ord) -- Ordering needed for S.Set
data SetVar = SetVar Integer
            | HSetVar {apos :: Int, id :: Integer}
            | TSetVar {apos :: Int, left :: SetVar, right :: SetVar}
            | SubRest {apos :: Int, inside :: SetVar}
            | SGSplit {apos :: Int, inside :: SetVar, isGroundNotSet :: Bool}
            | RCarry  {outside :: SetVar, insideExpr :: Expr}
                                                deriving (Ord)

data Var  = Var Char Integer | Meta Char Integer deriving (Eq,Ord)

data Token = V Var | B Bind | E Expr deriving (Eq,Ord,Show)


instance Eq SetVar where
    SetVar i == SetVar j = i == j
    HSetVar a i == HSetVar b j = a==b && i==j
    TSetVar i l r == TSetVar j m s = i==j && (l==m && r==s) || (l==s && r==m)
    SubRest a x == SubRest b y = a == b && x == y
    SGSplit a i x == SGSplit b j y = a == b && i == j && x == y
    RCarry o i == RCarry p j = o == p && i == j
    _ == _ = False

instance Show Bind where
    show (s := t) = show s++"="++show t

instance Show Var where
    show (Meta c i) = c:if i==0 then [] else show i
    show (Var  c i) = c:if i==0 then [] else show i

instance Show SetVar where
    show (SetVar i) = 'M':if i==0 then [] else show i
    show (HSetVar a i) = show (SetVar i) ++ replicate a '\''
    show (TSetVar a l r) = 'T':show (l,r) ++ replicate a '\''
    show (SubRest a x) = 'N':'(':(show x ++ ")" ++ replicate a '\'')
    show (SGSplit a i x) = (if x then "Gr" else "St") ++ show i ++ replicate a '\''
    show (RCarry o i) = show o ++ "<C>"

instance Show Expr where
    show (SetExpr s e) = case (null s, null e) of
        (True ,   _  ) -> show e
        (  _  ,   _  ) -> intercalate ";" (toList $ fmap show s) ++ ":" ++ show e

instance Ord Expr where
    compare (SetExpr s e) (SetExpr s' e') =
        compare e e' `mappend` compare s s'

instance Monoid Expr where
    mempty = SetExpr mempty mempty
    mappend (SetExpr s e) (SetExpr s' e') = SetExpr (mappend s s') (mappend e e')


------------------------------------------------
-- Constructors
------------------------------------------------

expr :: [Bind] -> Expr
expr = SetExpr mempty . fromList

setExpr :: [SetVar] -> [Bind] -> Expr
setExpr s e = SetExpr (fromList s) (fromList e)

partition :: SetVars -> [(SetVar, Int, SetVar)]
partition svs = map (\(v,oc) -> (v,oc,SubRest 0 v)) (toOccList svs)

isPartitionExpr :: Expr -> Bool
isPartitionExpr (SetExpr s _) = (length s == 1) && (case B.head s of SubRest _ _ -> True
                                                                     _ -> False)


meta :: Char -> Integer -> Var
meta c i  = if isUpper c then Meta c i else error "meta variable must be upper case"

var  :: Char -> Integer -> Var
var  c i  = if isLower c then Var  c i else error "variable must be lower case"

addApos :: SetVar -> SetVar
addApos (SetVar i) = HSetVar 1 i
addApos (HSetVar a i) = HSetVar (a+1) i
addApos (TSetVar a l r) = TSetVar (a+1) l r
addApos (SubRest a x) = SubRest (a+1) x
addApos (SGSplit a i x) = SGSplit (a+1) i x
addApos (RCarry o i) = RCarry (addApos o) i

-- | Splits the variable in the ground and set parts.
sgSplit :: SetVar -> (SetVar, SetVar)
sgSplit v = (SGSplit 0 v False, SGSplit 0 v True)

-- | Create carrying variable.
prepareRec :: SetVar -> Expr -> SetVar
prepareRec = RCarry

-- | Unwrap carried variable.
stopRec :: SetVar -> Expr
stopRec (RCarry o i) = i
stopRec v = error $ "called on non-carrying variable: " ++ show v

isCarrying :: SetVar -> Bool
isCarrying (RCarry _ _) = True
isCarrying _ = False

isMeta :: Var -> Bool
isMeta (Meta _ _) = True
isMeta _         = False

combine :: SetVar -> SetVar -> SetVar
combine = TSetVar 0


------------------------------------------------
-- Decomposition
------------------------------------------------

decompose :: Expr -> (SetVars, Binds)
decompose (SetExpr vs bs) = (vs,bs)


------------------------------------------------
-- Functions
------------------------------------------------

eConsS :: SetVar -> Expr -> Expr
eConsS s = bindsToExprS' (B.cons s)

eNull :: Expr -> Bool
eNull = bindsToExpr null

eNullS :: Expr -> Bool
eNullS = bindsToExprS null

eLength :: Expr -> Int
eLength = bindsToExpr length

eLengthS :: Expr -> Int
eLengthS = bindsToExprS length

eHead :: Expr -> Bind
eHead = bindsToExpr B.head

eHeadS :: Expr -> SetVar
eHeadS = bindsToExprS B.head

eTail :: Expr -> Expr
eTail = bindsToExpr' B.tail

eDelete :: Int -> Expr -> Expr
eDelete i = bindsToExpr' (B.deleteAt i)

eDeleteS :: Int -> Expr -> Expr
eDeleteS i = bindsToExprS' (B.deleteAt i)

clean :: Expr -> Expr -> (Expr, Expr)
clean (SetExpr s1 e1) (SetExpr s2 e2) =
    (SetExpr (s1 `B.diff` s2) e1, SetExpr (s2 `B.diff` s1) e2)

disjointS :: Expr -> Expr -> Bool
disjointS (SetExpr s1 e1) (SetExpr s2 e2) = disjoint s1 s2


------------------------------------------------
-- Canonical Extensions / Folds
------------------------------------------------

ωBind :: (Var -> Var) -> (Bind -> Bind)
ωBind f (v1:=v2) = f v1 := f v2

ωExpr :: (Var -> Var) -> (Expr -> Expr)
ωExpr f (SetExpr s e) = SetExpr s (fmap (ωBind f) e)

bindsToExpr :: (Binds -> a) -> (Expr -> a)
bindsToExpr f (SetExpr _ bs) = f bs

bindsToExpr' :: (Binds -> Binds) -> (Expr -> Expr)
bindsToExpr' f (SetExpr sv bs) = SetExpr sv (f bs)

bindsToExprS :: (SetVars -> a) -> (Expr -> a)
bindsToExprS f (SetExpr sv bs) = f sv

bindsToExprS' :: (SetVars -> SetVars) -> (Expr -> Expr)
bindsToExprS' f (SetExpr sv bs) = SetExpr (f sv) bs

foldWithIndex :: Monoid m => (Int -> Bind -> m) -> Expr -> m
foldWithIndex f (SetExpr _ e) = foldMapWithIndex f e

foldWithIndexSet :: Monoid m => (Int -> SetVar -> m) -> Expr -> m
foldWithIndexSet f (SetExpr s _) = foldMapWithIndex f s
