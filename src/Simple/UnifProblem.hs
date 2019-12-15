module Simple.UnifProblem
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
import qualified Data.MultiSet as MS

import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Token(E,B,V)
    )
import Simple.Substitution
    ( Substitution
    , onAny
    , onExpr
    )


------------------------------------------------
-- Data Types
------------------------------------------------

type UnifProblem  = S.Set UnifProblemEl
data UnifProblemEl = Expr :=.: Expr deriving (Eq,Ord)

type SolverDS = S.Set Equation
data Equation = Token :=?: Token deriving (Eq,Ord,Show)


instance Show UnifProblemEl where
    show (e1 :=.: e2) = show e1 ++ " =. " ++ show e2

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

isBlockEq :: Equation -> Bool
isBlockEq (E e1 :=?: E e2) = let (ms,_) = decompose e1 in
        length (partition ms) == 1 && isPartitionExpr e2
isBlockEq _ = False


------------------------------------------------
-- Substitution Application
------------------------------------------------

onEq :: Substitution -> Equation -> Equation
onEq σ (t1 :=?: t2) = (σ `onAny` t1) :=?: (σ `onAny` t2)

onEqP :: Substitution -> UnifProblemEl -> UnifProblemEl
onEqP σ (t1 :=.: t2) = (σ `onExpr` t1) :=.: (σ `onExpr` t2)

onSolver :: Substitution -> SolverDS -> SolverDS
onSolver σ = S.map (σ `onEq`)

onProblem :: Substitution -> UnifProblem -> SolverDS
onProblem σ = (σ `onSolver`) . probToSolver


------------------------------------------------
-- Checks
------------------------------------------------

check :: UnifProblemEl -> Bool
check (e1 :=.: e2) = e1 == e2

solves :: Substitution -> UnifProblem -> Bool
solves σ = all (check . (σ `onEqP`))
