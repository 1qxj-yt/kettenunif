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
    , eHead
    , eTail
    , eDelete
    , foldWithIndex
    -- * Variables
    , meta
    , var
    , isMeta
    , addApos
    , ωBind
    , ωExpr
    ) where

import Simple.Binds as B

import Data.Char(isUpper,isLower)

------------------------------------------------
-- Data
------------------------------------------------

data Expr = Expr Binds | SingleSVarExpr SetVar Binds | SetExpr SetVars Binds  deriving (Eq)
type SetVars = Multiset SetVar
type Binds = Multiset Bind

data Bind = Var := Var deriving (Eq,Ord) -- Ordering needed for S.Set
data SetVar = SetVar Integer | HSetVar {apos :: Int, id :: Integer} deriving (Eq,Ord)

data Var  = Var Char Integer | Meta Char Integer deriving (Eq,Ord)

data Token = V Var | B Bind | E Expr deriving (Eq,Ord,Show)


instance Show Bind where
    show (s := t) = show s++"="++show t

instance Show Var where
    show (Meta c i) = c:if i==0 then [] else show i
    show (Var  c i) = c:if i==0 then [] else show i

instance Show SetVar where
    show (SetVar i) = 'M':if i==0 then [] else show i
    show (HSetVar a i) = show (SetVar i) ++ replicate a '\''

instance Show Expr where
    show (Expr e) = show e
    show (SingleSVarExpr sv e) = show sv++":"++show e

instance Ord Expr where
    compare (Expr e) (Expr e') = compare e e'
    compare (Expr _) (SingleSVarExpr _ _) = LT
    compare (SingleSVarExpr _ _) (Expr _) = GT
    compare (SingleSVarExpr s e) (SingleSVarExpr s' e') =
        compare e e' `mappend` compare s s'


------------------------------------------------
-- Constructors
------------------------------------------------

expr :: [Bind] -> Expr
expr = Expr . fromList

ssve :: SetVar -> [Bind] -> Expr
ssve sv = SingleSVarExpr sv . fromList

meta :: Char -> Integer -> Var
meta c i  = if isUpper c then Meta c i else error "meta variable must be upper case"

var  :: Char -> Integer -> Var
var  c i  = if isLower c then Var  c i else error "variable must be lower case"

addApos :: SetVar -> SetVar
addApos (SetVar i) = HSetVar 1 i
addApos (HSetVar a i) = HSetVar (a+1) i

isMeta :: Var -> Bool
isMeta (Meta _ _) = True
isMeta _         = False


------------------------------------------------
-- Functions
------------------------------------------------

eHead :: Expr -> Bind
eHead = bindsToExpr B.head

eTail :: Expr -> Expr
eTail = bindsToExpr' B.tail

eDelete :: Int -> Expr -> Expr
eDelete i = bindsToExpr' (B.deleteAt i)


------------------------------------------------
-- Canonical Extensions / Folds
------------------------------------------------

ωBind :: (Var -> Var) -> (Bind -> Bind)
ωBind f (v1:=v2) = f v1 := f v2

ωExpr :: (Var -> Var) -> (Expr -> Expr)
ωExpr f (Expr e) = Expr (fmap (ωBind f) e)
ωExpr f (SingleSVarExpr b e) = SingleSVarExpr b (fmap (ωBind f) e)

bindsToExpr :: (Binds -> a) -> (Expr -> a)
bindsToExpr f (Expr bs) = f bs
bindsToExpr f (SingleSVarExpr _ bs) = f bs

bindsToExpr' :: (Binds -> Binds) -> (Expr -> Expr)
bindsToExpr' f (Expr bs) = Expr (f bs)
bindsToExpr' f (SingleSVarExpr sv bs) = SingleSVarExpr sv (f bs)

foldWithIndex :: Monoid m => (Int -> Bind -> m) -> Expr -> m
foldWithIndex f (Expr e) = foldMapWithIndex f e
foldWithIndex f (SingleSVarExpr _ e) = foldMapWithIndex f e
