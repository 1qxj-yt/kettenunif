module Simple.Algorithm
    ( solve
    , generalSolver
    ) where

import Simple.Substitution
    ( Substitution
    , identity
    , compose
    , restrict
    )
import Simple.UnifProblem
    ( UnifProblem
    , SolverDS
    , probToSolver
    )
import Simple.Rules
    ( Rule(apply)
    , Input
    , Output
    , SSList(SSL)
    )
import Simple.RuleSwitches

import qualified Data.Set as S(null,deleteFindMin)
import Control.Monad.Identity


solve :: UnifProblem -> [Substitution]
solve = map sslToSubst. (\(Identity x) -> x) . run

sslToSubst :: SSList -> Substitution
sslToSubst (SSL list) = restrict $ foldr compose identity list

run :: UnifProblem -> Identity [SSList]
run = generalSolver (\_ _ -> return ()) (\_ _ _ -> return ()) id ()


------------------------------------------------
-- General Solver
------------------------------------------------

generalSolver :: Monad m => (a -> SSList -> m ()) -> (a -> Input -> Rule -> m ())
                        -> (a -> a) -> a -> UnifProblem -> m [SSList]
generalSolver t r u a = generalSolverRec t r u a . ini
    where
        ini :: UnifProblem -> (SSList, SolverDS)
        ini prob = (SSL [identity], probToSolver prob)

generalSolverRec :: Monad m => (a -> SSList -> m ()) -> (a -> Input -> Rule -> m ())
                        -> (a -> a) -> a -> Output -> m [SSList]
generalSolverRec onTerm onRule update args (sol,γ)
    | S.null γ = do
        onTerm args sol
        return [sol]
    | otherwise =
        let (eq,γ') = S.deleteFindMin γ
            rule    = fullMSet eq
            nextLs  = apply rule (sol, eq, γ')
            input  = (sol, eq, γ')
        in do
            onRule args input rule
            concat <$> sequence [generalSolverRec onTerm onRule update (update args) next | next <- nextLs ]
