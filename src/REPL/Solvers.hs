module REPL.Solvers
    ( silent
    , verbose
    , counting
    ) where

import Simple.Algorithm(solve,generalSolver)
import Simple.UnifProblem(UnifProblem)
import Simple.Rules(SSList(SSL),Input,Output,Rule,name)
import Simple.Substitution(Substitution,compose,identity,restrict)

import Control.Monad.Writer
import Control.Monad.State
import Data.List(intercalate)


------------------------------------------------
-- Silent Solver
------------------------------------------------

silent :: UnifProblem -> [Substitution]
silent = solve

sslToSubst :: SSList -> Substitution
sslToSubst (SSL l) = foldr compose identity l

------------------------------------------------
-- Verbose Solver
------------------------------------------------

data StepInfo = S (Int,Input,Rule) | T (Int,SSList)

verbose :: UnifProblem -> String
verbose prob = let (rs,lg) = runSolverWriter prob
        in intercalate "\n" (map printInfo lg)
            ++ '\n':show (map (\(SSL l) -> foldr compose identity l) rs)

printInfo :: StepInfo -> String
printInfo (S (n,(SSL sol,eq,γ),rule)) =
    let indent = replicate (2*n) ' '
    in  indent ++ "(" ++ show (foldr compose identity sol) ++", "++ show eq ++" ∪ Γ)" ++ "\n"
                ++ indent ++ name rule
printInfo (T (n,SSL sol)) =
    let indent = replicate (2*n) ' '
    in  indent ++ "** " ++ show (foldr compose identity sol) ++ " **"

runSolverWriter :: UnifProblem -> ([SSList], [StepInfo])
runSolverWriter = runWriter . solveLogger 0

solveLogger :: Int -> UnifProblem -> Writer [StepInfo] [SSList]
solveLogger = generalSolver
                    (\n sol -> tell [T (n,sol)])
                    (\n inp rule -> tell [S (n,inp,rule)])
                    succ


------------------------------------------------
-- Counting Solver
------------------------------------------------

counting :: UnifProblem -> String
counting prob = let ((_,num),str) = runWriter $ runStateT (solveCountSW prob) 0
            in str ++ '[':show num++"]"

solveCountSW :: UnifProblem -> StateT Integer (Writer String) [SSList]
solveCountSW = generalSolver
                (\_ sol -> do
                    n <- get
                    lift . tell $ replicate (length $ show n) '\b'
                    modify (+1)
                    n <- get
                    lift . tell $ show n
                )
                (\_ _ _ -> return ())
                id ()
