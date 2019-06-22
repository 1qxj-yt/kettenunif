module Expression
    ( Expr
    , Bind((:=))
    , Var
    , Token(B,V,E)
    , meta
    , var
    , isMeta
    ) where


import Data.Char(isUpper,isLower)

------------------------------------------------
-- Data
------------------------------------------------

type Expr = [Bind]

data Bind = Var := Var | SetVar Integer | ChVar Integer deriving (Eq,Ord) -- Ordering needed for S.Set

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
