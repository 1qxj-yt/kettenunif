module Simple.Expression
    ( Expr(Expr)
    , Bind((:=))
    , Var
    , Token(B,V,E)
    , meta
    , var
    , isMeta
    , ωBind
    , ωExpr
    ) where


import Data.Char(isUpper,isLower)

------------------------------------------------
-- Data
------------------------------------------------

data Expr = Expr [Bind] | SingleSVarExpr SetVar [Bind] deriving (Eq,Ord,Show)

data Bind = Var := Var deriving (Eq,Ord) -- Ordering needed for S.Set
newtype SetVar = SetVar Integer deriving (Eq,Ord,Show)

data Var  = Var Char Integer | Meta Char Integer deriving (Eq,Ord)

data Token = B Bind | V Var | E Expr deriving (Eq,Ord,Show)


------------------------------------------------
-- Constructors
------------------------------------------------

meta :: Char -> Integer -> Var
meta c i  = if isUpper c then Meta c i else error "meta variable must be upper case"

var  :: Char -> Integer -> Var
var  c i  = if isLower c then Var  c i else error "variable must be lower case"


isMeta :: Var -> Bool
isMeta (Meta _ _) = True
isMeta _         = False


------------------------------------------------
-- Show Instances
------------------------------------------------

instance Show Bind where
    show (s := t) = show s++"="++show t

instance Show Var where
    show (Meta c i) = c:if i==0 then [] else show i
    show (Var  c i) = c:if i==0 then [] else show i


------------------------------------------------
-- Canonical Extensions
------------------------------------------------

ωBind :: (Var -> Var) -> (Bind -> Bind)
ωBind f (v1:=v2) = f v1 := f v2

ωExpr :: (Var -> Var) -> (Expr -> Expr)
ωExpr f (Expr e) = Expr (map (ωBind f) e)
