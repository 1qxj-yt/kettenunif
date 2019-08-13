module Simple.Algorithm
    ( solve
    , solveVerbose
    ) where

import Simple.Expression
    ( Expr(Expr)
    , Token(E,B,V)
    , isMeta
    )
import Simple.Substitution
    ( Substitution
    , identity
    , compose
    , build
    )
import Simple.UnifProblem
    ( UnifProblem
    , SolverDS
    , Equation((:=?:))
    , probToSolver
    )
import Simple.Rules
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
import Data.List(intercalate)

type StepInfo = (Int,Input,Rule)

------------------------------------------------
-- Silent Solver
------------------------------------------------

solve :: UnifProblem -> [Substitution]
solve prob = map sslToSubst . fst $ runSolverWriter prob

sslToSubst :: SSList -> Substitution
sslToSubst (SSL list) = foldr compose identity list

------------------------------------------------
-- Verbose Solver
------------------------------------------------

solveVerbose :: UnifProblem -> String
solveVerbose prob = intercalate "\n" $  map printInfo . snd $ runSolverWriter prob

printInfo :: StepInfo -> String
printInfo (n,(SSL sol,eq,γ),rule) =
    let indent = replicate n ' '
    in  indent ++ "(" ++ show (build sol) ++", "++ show eq ++" ∪ Γ)" ++ "\n"
            ++ indent ++ name rule

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
ruleFor (E (Expr []) :=?: E (Expr [])) = tautology
ruleFor (E (Expr e1) :=?: E (Expr e2)) = if length e1 == length e2 then distribution else clash
ruleFor (V v1 :=?: V v2)
    | v1 == v2  = tautology
    | otherwise = case (isMeta v1, isMeta v2) of
        (False, False) -> clash
        (True , False) -> application
        (False, True ) -> orientation
        (True , True ) -> application
