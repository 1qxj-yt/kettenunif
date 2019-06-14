module Substitution
    ( Substitution
    , Token(..)
    , (→)
    , isWellDef
    , onAny
    ) where

import Expression
    ( Expr
    , Bind((:=))
    , Var, var, meta
    , isMeta
    )

import Data.List(find,nub)

------------------------------------------------
-- Data Types
------------------------------------------------

data SingleSubst    = Subst{tup::(Var,Var)} deriving Eq
type Substitution   = [SingleSubst]

instance Show SingleSubst where
    show (Subst (v1,v2)) = show v1++"→"++(show v2)

-- Constructor
infixl →
(→) :: Var -> Var -> SingleSubst
v1 → v2 = if isMeta v1
    then Subst (v1,v2)
    else error "substitution origin is non-meta"

isWellDef :: Substitution -> Bool
isWellDef σ = let origins = map (fst.tup) σ
            in  length origins == length (nub origins) 
                && all isMeta origins


------------------------------------------------
-- Substitution Application
------------------------------------------------

onVar :: Substitution -> (Var -> Var)
onVar σ v1 = case find ((==v1).fst.tup) σ of
    Nothing -> v1
    Just x  -> (snd.tup) x

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
