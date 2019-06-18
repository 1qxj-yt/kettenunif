module Algorithm
    ( solve
    ) where

import Expression
    ( isMeta
    )
import Substitution
    ( Substitution
    , Token(E,B,V)
    , identity
    , compose
    )
import UnifProblem
    ( UnifProblem
    , SolverDS
    , Equation((:=?:))
    , probToSolver
    )
import Rules
    ( Rule(name,apply)
    , Input
    , Output
    , SSList(SSL)
    , tautology
    , clash
    , distribution
    , decomposition
    , application
    , orientation
    )

import qualified Data.Set as S(null,deleteFindMin)
import Control.Monad.Writer

type StepInfo = (Int,Input,Rule)

------------------------------------------------
-- Silent Solver
------------------------------------------------

solve :: UnifProblem -> [Substitution]
solve prob = map sslToSubst . fst $ runSolverWriter prob

sslToSubst :: SSList -> Substitution
sslToSubst (SSL list) = foldr compose identity list
------------------------------------------------
-- General Solver
------------------------------------------------

runSolverWriter :: UnifProblem -> ([SSList], [StepInfo])
runSolverWriter prob = runWriter $ solveGeneral 0 (SSL [identity], probToSolver prob)

solveGeneral :: Int -> Output -> Writer [StepInfo] [SSList]
solveGeneral n (sol,γ)
    | S.null γ = return [sol]
    | otherwise=
        let (eq,γ') = S.deleteFindMin γ
            rule    = ruleFor eq
            nextLs  = apply rule (sol, eq, γ')

            input  = (sol, eq, γ')
        in do
            tell [(n,input,rule)]
            concat <$> sequence [solveGeneral (succ n) next | next <- nextLs ]


------------------------------------------------
-- Selecting the Right Rule
------------------------------------------------

ruleFor :: Equation -> Rule
ruleFor (B _ :=?: B _)   = decomposition
ruleFor (E [] :=?: E []) = tautology
ruleFor (E e1 :=?: E e2) = if length e1 == length e2 then distribution else clash
ruleFor (V v1 :=?: V v2)
    | v1 == v2  = tautology
    | otherwise = case (isMeta v1, isMeta v2) of
        (False, False) -> if v1 == v2 then tautology else clash
        (True , False) -> application
        (False, True ) -> orientation
        (True , True ) -> if v1 == v2 then tautology else application
