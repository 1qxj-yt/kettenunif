module Algorithm where

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
    ( Rule
    , tautology
    , clash
    , distribution
    , decomposition
    , application
    , orientation
    )

import qualified Data.Set as S(null,deleteFindMin)

solve :: UnifProblem -> [Substitution]
solve prob = solveAux ([identity], probToSolver prob)

solveAux :: ([Substitution], SolverDS) -> [Substitution]
solveAux (sol,γ)
    | S.null γ  = [foldr compose identity sol]
    | otherwise = let (eq,γ') = S.deleteFindMin γ
                      nextLs = applyRuleFor eq (sol, eq, γ')
                  in concat [solveAux next | next <- nextLs ]


applyRuleFor :: Equation -> Rule
applyRuleFor (B _ :=?: B _)   = decomposition
applyRuleFor (E [] :=?: E []) = tautology
applyRuleFor (E e1 :=?: E e2) = if length e1 == length e2 then distribution else clash
applyRuleFor (V v1 :=?: V v2)
    | v1 == v2  = tautology
    | otherwise = case (isMeta v1, isMeta v2) of
        (False, False) -> if v1 == v2 then tautology else clash
        (True , False) -> application
        (False, True ) -> orientation
        (True , True ) -> if v1 == v2 then tautology else application
