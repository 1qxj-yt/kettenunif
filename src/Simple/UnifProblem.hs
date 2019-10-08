module Simple.UnifProblem
    ( UnifProblem
    , UnifProblemEl((:=.:))
    , SolverDS
    , Equation((:=?:))
    , probToSolver
    , onSolver
    , onProblem
    , solves
    -- * Equation Operations
    , selectEq
    -- * SolverDS Records
    , equations
    , duplicateAvoidance
    -- * Duplicate Avoidance
    , canTerminate
    , daContainsDups
    , cleanUpDA
    ) where

import qualified Data.Set as S
import qualified Data.MultiSet as MS

import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Token(E,B,V)
    , Var
    , isMeta
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

data SolverDS = SDS {
        equations :: S.Set Equation
      , duplicateAvoidance :: S.Set (MS.MultiSet Var)
    }
data Equation = Token :=?: Token deriving (Eq,Ord,Show)


instance Show UnifProblemEl where
    show (e1 :=.: e2) = show e1 ++ " =. " ++ show e2

probToSolver :: UnifProblem -> SolverDS
probToSolver = ($ S.empty) . SDS . S.map probToSolver'
    where
        probToSolver' :: UnifProblemEl -> Equation
        probToSolver' (e1 :=.: e2) = E e1 :=?: E e2

isValidEquation :: Equation -> Bool
isValidEquation (E _ :=?: E _) = True
isValidEquation (B _ :=?: B _) = True
isValidEquation (V _ :=?: V _) = True
isValidEquation _ = False

isValidSolver :: SolverDS -> Bool
isValidSolver = (== True) . S.findMin . S.map isValidEquation . equations

selectEq :: SolverDS -> (Equation, SolverDS)
selectEq ds = let (eq,eqs) = S.deleteFindMin (equations ds)
            in (eq, ds{equations=eqs} )

canTerminate :: SolverDS -> Bool
canTerminate ds = S.null (equations ds) && not (daContainsDups ds)

daContainsDups :: SolverDS -> Bool
daContainsDups = not . S.null . S.filter (not.isDistinct) . duplicateAvoidance

isDistinct :: MS.MultiSet a -> Bool
isDistinct ms = MS.distinctSize ms == MS.size ms

cleanUpDA :: SolverDS -> SolverDS
cleanUpDA ds = ds { duplicateAvoidance =
            S.filter (\ms -> not (allGround ms && isDistinct ms)) (duplicateAvoidance ds)
        }

allGround :: MS.MultiSet Var -> Bool
allGround = all (not . isMeta)


------------------------------------------------
-- Substitution Application
------------------------------------------------

onEq :: Substitution -> Equation -> Equation
onEq σ (t1 :=?: t2) = (σ `onAny` t1) :=?: (σ `onAny` t2)

onEqP :: Substitution -> UnifProblemEl -> UnifProblemEl
onEqP σ (t1 :=.: t2) = (σ `onExpr` t1) :=.: (σ `onExpr` t2)

onSolver :: Substitution -> SolverDS -> SolverDS
onSolver σ ds = ds {equations = S.map (σ `onEq`) (equations ds)}

onProblem :: Substitution -> UnifProblem -> SolverDS
onProblem σ = (σ `onSolver`) . probToSolver


------------------------------------------------
-- Checks
------------------------------------------------

check :: UnifProblemEl -> Bool
check (e1 :=.: e2) = e1 == e2

solves :: Substitution -> UnifProblem -> Bool
solves σ = all (check . (σ `onEqP`))
