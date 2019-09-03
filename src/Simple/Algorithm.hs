module Simple.Algorithm
    ( solve
    , solveVerbose
    ) where

import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Token(E,B,V)
    , isMeta
    )
import Simple.Substitution
    ( Substitution
    , identity
    , compose
    , restrict
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
    , termination
    , tautology
    , clash
    , distribution
    , decomposition
    , application
    , orientation
    , set_distribution
    , set_application
    , biset_tautology
    , biset_application
    , biset_distribution
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
sslToSubst (SSL list) = restrict $ foldr compose identity list

------------------------------------------------
-- Verbose Solver
------------------------------------------------

solveVerbose :: UnifProblem -> String
solveVerbose prob = let (rs,lg) = runSolverWriter prob
        in intercalate "\n" (map printInfo lg)
            ++ '\n':show (map (\(SSL l) -> foldr compose identity l) rs)

printInfo :: StepInfo -> String
printInfo (n,(SSL sol,eq,γ),rule) =
    let indent = replicate (2*n) ' '
    in  case name rule of
            "termination" -> indent ++ "** " ++ show (foldr compose identity sol) ++ " **"
            str ->
                indent ++ "(" ++ show (foldr compose identity sol) ++", "++ show eq ++" ∪ Γ)" ++ "\n"
                ++ indent ++ name rule

------------------------------------------------
-- General Solver
------------------------------------------------

runSolverWriter :: UnifProblem -> ([SSList], [StepInfo])
runSolverWriter prob = runWriter $ solveGeneral 0 (SSL [identity], probToSolver prob)

solveGeneral :: Int -> Output -> Writer [StepInfo] [SSList]
solveGeneral n (sol,γ)
    | S.null γ = do
        tell [(n,(sol,E (Expr []) :=?: E (Expr []) ,γ), termination)]
        return [sol]
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
ruleFor (E (SingleSVarExpr sv1 e1) :=?: E (SingleSVarExpr sv2 e2))
                | sv1 == sv2
                    && null e1
                    && null e2  = biset_tautology
                | null e2       = orientation
                | null e1       = biset_application
                | otherwise     = biset_distribution
ruleFor (E (SingleSVarExpr sv []) :=?: E e) = set_application
ruleFor (E e :=?: E (SingleSVarExpr sv e2)) = orientation
ruleFor (E (SingleSVarExpr sv e1) :=?: E e) = set_distribution
ruleFor (V v1 :=?: V v2)
    | v1 == v2  = tautology
    | otherwise = case (isMeta v1, isMeta v2) of
        (False, False) -> clash
        (True , False) -> application
        (False, True ) -> orientation
        (True , True ) -> application
