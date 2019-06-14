module Substitution
    ( Substitution
    , Token(..)
    , (→)
    , onAny
    ) where

import Expression
    ( Expr
    , Bind((:=))
    , Var, var, meta
    , isMeta
    )


------------------------------------------------
-- Data Types
------------------------------------------------


data SingleSubst    = Subst {fun :: Var -> Var, tup :: (Var, Var)}
type Substitution   = [SingleSubst]

instance Show SingleSubst where
    show Subst{tup=(v1,v2)}  = show v1++"→"++(show v2) 


-- Constructor
infixl →
(→) :: Var -> Var -> SingleSubst
(→) bef aft = if isMeta bef
    then Subst (\v -> if v == bef then aft else v) (bef,aft)
    else error "substitution origin is non-meta"


------------------------------------------------
-- Substitution Application
------------------------------------------------

onVar :: Substitution -> (Var -> Var)
onVar σ = foldl (.) id (map fun σ)

onBind :: Substitution -> (Bind -> Bind)
onBind σ (v1:=v2) = σ `onVar` v1 := (σ `onVar` v2)

onExpr :: Substitution -> (Expr -> Expr)
onExpr σ = map (σ `onBind`)


------------------------------------------------
-- Generalization
------------------------------------------------

data Token = B Bind | V Var | E Expr deriving (Eq,Ord,Show)

onAny :: Substitution -> Token -> Token
onAny σ t = case t of
                E e -> E (σ `onExpr` e)
                B b -> B (σ `onBind` b)
                V v -> V (σ `onVar`  v)
