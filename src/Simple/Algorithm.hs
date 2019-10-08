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
    , selectEq
    , canTerminate
    , daContainsDups
    , cleanUpDA
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

-- | @generalSolver onTerm onRule updater args problem@ returns a list of
-- Single Substitution Lists in a monad @m@, solving the @problem@.
-- @args@ is an argument of arbitrary type, which is updated by
-- @updater@ on every recursion step.
-- @onRule@ is called on every rule application with the updated argument @a@,
-- the Input provided to the Rule as well as the Rule itself.
-- @onTerm@ is called on termination with the updateded argument @a@ and the
-- result SSList.
generalSolver :: Monad m => (a -> SSList -> m ()) -> (a -> Input -> Rule -> m ())
                        -> (a -> a) -> a -> UnifProblem -> m [SSList]
generalSolver t r u a = generalSolverRec t r u a . ini
    where
        ini :: UnifProblem -> (SSList, SolverDS)
        ini prob = (SSL [identity], probToSolver prob)

generalSolverRec :: Monad m => (a -> SSList -> m ()) -> (a -> Input -> Rule -> m ())
                        -> (a -> a) -> a -> Output -> m [SSList]
generalSolverRec onTerm onRule update args (sol,γ)
    | canTerminate γ = do
        onTerm args sol
        return [sol]
    | daContainsDups γ = return []
    | otherwise =
        let (eq,γ') = selectEq γ
            rule    = fullMSet eq
            nextLs  = apply rule (sol, eq, γ')
            input  = (sol, eq, γ')
        in do
            onRule args input rule
            concat <$> sequence [generalSolverRec onTerm onRule update (update args) next | next <- nextLs ]
