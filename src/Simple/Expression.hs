module Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Binds
    , Bind((:=))
    , Var
    , SetVar(SetVar)
    , Token(B,V,E)
    -- * Expression
    , expr
    , ssve
    , setExpr
    , eConsS
    , eNull
    , eNullS
    , ePartitionTo
    , ePartitionWithRestTo
    , eHead
    , eTail
    , eDelete
    , eDeleteS
    , clean
    , disjointS
    , foldWithIndex
    , foldWithIndexSet
    -- * Variables
    , meta
    , var
    , isMeta
    , addApos
    , combine
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

data Expr = Expr Binds | SingleSVarExpr SetVar Binds | SetExpr SetVars Binds  deriving (Eq)
type SetVars = Multiset SetVar
type Binds = Multiset Bind

data Bind = Var := Var deriving (Eq,Ord) -- Ordering needed for S.Set
data SetVar = SetVar Integer
            | HSetVar {apos :: Int, id :: Integer}
            | TSetVar {apos :: Int, left :: SetVar, right :: SetVar}
                                                deriving (Ord)

data Var  = Var Char Integer | Meta Char Integer deriving (Eq,Ord)

data Token = V Var | B Bind | E Expr deriving (Eq,Ord,Show)


instance Eq SetVar where
    SetVar i == SetVar j = i == j
    HSetVar a i == HSetVar b j = a==b && i==j
    TSetVar i l r == TSetVar j m s = i==j && (l==m && r==s) || (l==s && r==m)
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

instance Show Expr where
    show (Expr e) = show e
    show (SingleSVarExpr sv e) = show sv++":"++show e
    show (SetExpr s e) = case (null s, null e) of
        (True ,   _  ) -> show e
        (  _  ,   _  ) -> intercalate ";" (toList $ fmap show s) ++ ":" ++ show e

instance Ord Expr where
    compare (Expr e) (Expr e') = compare e e'
    compare (Expr _) (SingleSVarExpr _ _) = LT
    compare (SingleSVarExpr _ _) (Expr _) = GT
    compare (SingleSVarExpr s e) (SingleSVarExpr s' e') =
        compare e e' `mappend` compare s s'
    compare (SetExpr s e) (SetExpr s' e') =
        compare e e' `mappend` compare s s'

instance Monoid Expr where
    mempty = SetExpr mempty mempty
    mappend (SetExpr s e) (SetExpr s' e') = SetExpr (mappend s s') (mappend e e')


------------------------------------------------
-- Constructors
------------------------------------------------

expr :: [Bind] -> Expr
expr = Expr . fromList

setExpr :: [SetVar] -> [Bind] -> Expr
setExpr s e = SetExpr (fromList s) (fromList e)

ssve :: SetVar -> [Bind] -> Expr
ssve sv = SingleSVarExpr sv . fromList

meta :: Char -> Integer -> Var
meta c i  = if isUpper c then Meta c i else error "meta variable must be upper case"

var  :: Char -> Integer -> Var
var  c i  = if isLower c then Var  c i else error "variable must be lower case"

addApos :: SetVar -> SetVar
addApos (SetVar i) = HSetVar 1 i
addApos (HSetVar a i) = HSetVar (a+1) i
addApos (TSetVar a l r) = TSetVar (a+1) l r

isMeta :: Var -> Bool
isMeta (Meta _ _) = True
isMeta _         = False

combine :: SetVar -> SetVar -> SetVar
combine = TSetVar 0


------------------------------------------------
-- Functions
------------------------------------------------

eConsS :: SetVar -> Expr -> Expr
eConsS s = bindsToExprS' (B.cons s)

eNull :: Expr -> Bool
eNull = bindsToExpr null

eNullS :: Expr -> Bool
eNullS = bindsToExprS null

eHead :: Expr -> Bind
eHead = bindsToExpr B.head

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
disjointS (SingleSVarExpr s1 _) (SingleSVarExpr s2 _) = s1 /= s2
disjointS (SetExpr s1 e1) (SetExpr s2 e2) = disjoint s1 s2
disjointS _ _ = True

ePartitionTo :: Expr -> Expr -> [SetVar -> Expr]
(SetExpr _ e) `ePartitionTo` (SetExpr m _)
    = map (SetExpr mempty .) (partitions m e)

ePartitionWithRestTo :: Expr -> Expr -> [(SetVar -> Expr, Expr)]
(SetExpr _ e) `ePartitionWithRestTo` (SetExpr m _)
    =  map ((SetExpr mempty .) Control.Arrow.*** SetExpr mempty) (partitionsWithRest m e)


------------------------------------------------
-- Canonical Extensions / Folds
------------------------------------------------

ωBind :: (Var -> Var) -> (Bind -> Bind)
ωBind f (v1:=v2) = f v1 := f v2

ωExpr :: (Var -> Var) -> (Expr -> Expr)
ωExpr f (Expr e) = Expr (fmap (ωBind f) e)
ωExpr f (SingleSVarExpr b e) = SingleSVarExpr b (fmap (ωBind f) e)
ωExpr f (SetExpr s e) = SetExpr s (fmap (ωBind f) e)

bindsToExpr :: (Binds -> a) -> (Expr -> a)
bindsToExpr f (Expr bs) = f bs
bindsToExpr f (SingleSVarExpr _ bs) = f bs
bindsToExpr f (SetExpr _ bs) = f bs

bindsToExpr' :: (Binds -> Binds) -> (Expr -> Expr)
bindsToExpr' f (Expr bs) = Expr (f bs)
bindsToExpr' f (SingleSVarExpr sv bs) = SingleSVarExpr sv (f bs)
bindsToExpr' f (SetExpr sv bs) = SetExpr sv (f bs)

bindsToExprS :: (SetVars -> a) -> (Expr -> a)
bindsToExprS f (SetExpr sv bs) = f sv

bindsToExprS' :: (SetVars -> SetVars) -> (Expr -> Expr)
bindsToExprS' f (SetExpr sv bs) = SetExpr (f sv) bs

foldWithIndex :: Monoid m => (Int -> Bind -> m) -> Expr -> m
foldWithIndex f (Expr e) = foldMapWithIndex f e
foldWithIndex f (SingleSVarExpr _ e) = foldMapWithIndex f e
foldWithIndex f (SetExpr _ e) = foldMapWithIndex f e

foldWithIndexSet :: Monoid m => (Int -> SetVar -> m) -> Expr -> m
foldWithIndexSet f (SetExpr s _) = foldMapWithIndex f s


------------------------------------------------
-- Chain Operations
------------------------------------------------

toMap :: Binds -> M.Map Var [Var]
toMap = M.fromListWith (++) . map (\(k := a) -> (k,[a])) . toList

findChains :: (Var,Var) -> M.Map Var [Var] -> [Expr]
findChains (k,a) mp = --traceShow mp $
    case M.lookup k mp of
        Nothing -> []
        Just as -> do
            a' <- as
            let mp' = M.update (\as ->
                        if length as == 1 then Nothing else Just (delete a' as)
                    ) k mp
            if M.null mp' && a==a' then return $ expr [k:=a']
                else do
                    let rcMp = findChains (a',a) mp'
                    Expr rc <- rcMp
                    return $ Expr $ B.cons (k:=a') rc
