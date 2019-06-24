module Extended.UnifProblem
    ( UnifProblem
    , UnifProblemEl((:=.:))
    , SolverDS
    , Equation((:=?:))
    , probToSolver
    , onSolver
    , onProblem
    , solves
    ) where

import qualified Data.Set as S

import Expression
    ( Expr
    , Token(E,B,V)
    )
import Substitution
    ( Substitution
    , onAny
    )


------------------------------------------------
-- Data Types
------------------------------------------------

type UnifProblem  = S.Set UnifProblemEl
data UnifProblemEl = Expr :=.: Expr deriving (Eq,Ord,Show)

type SolverDS = S.Set Equation
data Equation = Token :=?: Token deriving (Eq,Ord,Show)


probToSolver :: UnifProblem -> SolverDS
probToSolver = S.map probToSolver'
    where
        probToSolver' :: UnifProblemEl -> Equation
        probToSolver' (e1 :=.: e2) = E e1 :=?: E e2

isValidEquation :: Equation -> Bool
isValidEquation (E _ :=?: E _) = True
isValidEquation (B _ :=?: B _) = True
isValidEquation (V _ :=?: V _) = True
isValidEquation _ = False

isValidSolver :: SolverDS -> Bool
isValidSolver = (== True) . S.findMin . S.map isValidEquation


------------------------------------------------
-- Substitution Application
------------------------------------------------

onEq :: Substitution -> Equation -> Equation
onEq σ (t1 :=?: t2) = (σ `onAny` t1) :=?: (σ `onAny` t2)

onSolver :: Substitution -> SolverDS -> SolverDS
onSolver σ = S.map (σ `onEq`)

onProblem :: Substitution -> UnifProblem -> SolverDS
onProblem σ = (σ `onSolver`) . probToSolver


------------------------------------------------
-- Checks
------------------------------------------------

check :: Equation -> Bool
check (e1 :=?: e2) = e1 == e2

solves :: Substitution -> UnifProblem -> Bool
solves σ p = all check (σ `onProblem` p)
