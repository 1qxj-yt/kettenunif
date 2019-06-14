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

import Data.List(find)

------------------------------------------------
-- Data Types
------------------------------------------------

type SingleSubst    = (Var,Var)
type Substitution   = [SingleSubst]

-- Constructor
infixl →
(→) :: Var -> Var -> SingleSubst
v1 → v2 = (v1, v2)


------------------------------------------------
-- Substitution Application
------------------------------------------------

onVar :: Substitution -> (Var -> Var)
onVar σ v1 = case find ((==v1).fst) σ of
    Nothing     -> v1
    Just (_,v2) -> v2

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
