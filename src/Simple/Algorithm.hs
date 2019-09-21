module Simple.Algorithm
    ( solve
    , generalSolver
    ) where

import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Token(E,B,V)
    , isMeta
    , eNull
    , eNullS
    , disjointS
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

    , x_semi_tautology
    , x_distribution
    , x_application

    , set_distribution
    , set_application
    , biset_tautology
    , biset_application
    , biset_distribution
    , mset_semi_tautology
    )

import qualified Data.Set as S(null,deleteFindMin)
import Control.Monad.Identity
import Data.List(intercalate)



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

------------------------------------------------
-- Selecting the Right Rule
------------------------------------------------

type RuleSwitch = Equation -> Rule

fullMSet :: RuleSwitch
fullMSet (B _ :=?: B _)   = decomposition
fullMSet (V v1 :=?: V v2)
    | v1 == v2  = tautology
    | otherwise = case (isMeta v1, isMeta v2) of
        (False, False) -> clash
        (True , False) -> application
        (False, True ) -> orientation
        (True , True ) -> application
fullMSet (E e1 :=?: E e2)
    | not (disjointS e1 e2) = x_semi_tautology
    | otherwise             = case (eNull e1, eNull e2) of
                    (True , True ) -> x_application
                    (False, True ) -> orientation
                    (True , False) -> if eNullS e1
                                        then clash
                                        else x_application
                    (False, False) -> x_distribution


singleMSet :: RuleSwitch
singleMSet (B _ :=?: B _)   = decomposition
singleMSet (E (Expr e1) :=?: E (Expr e2)) = case (null e1, null e2) of
                    (True , True ) -> tautology
                    _ -> if length e1 == length e2 then distribution else clash
singleMSet (E (SingleSVarExpr sv1 e1) :=?: E (SingleSVarExpr sv2 e2))
                | sv1 == sv2    = case (null e1, null e2) of
                    (True , True ) ->   biset_tautology
                    (False, True ) ->   orientation
                    (True , False) ->   clash
                    (False, False) ->   mset_semi_tautology
                | otherwise     = case (null e1, null e2) of
                    (True , True ) ->   biset_application
                    (False, True ) ->   orientation
                    (True , False) ->   biset_application
                    (False, False) ->   biset_distribution
singleMSet (E e :=?: E (SingleSVarExpr sv e2)) = orientation
singleMSet (E (SingleSVarExpr sv e1) :=?: E e)
    | null e1   = set_application
    | otherwise = set_distribution
singleMSet (V v1 :=?: V v2)
    | v1 == v2  = tautology
    | otherwise = case (isMeta v1, isMeta v2) of
        (False, False) -> clash
        (True , False) -> application
        (False, True ) -> orientation
        (True , True ) -> application


singleTrueSet :: RuleSwitch
singleTrueSet (B _ :=?: B _)   = decomposition
singleTrueSet (E (Expr e1) :=?: E (Expr e2)) = case (null e1, null e2) of
                    (True , True ) -> tautology
                    _ -> if length e1 == length e2 then distribution else clash
singleTrueSet (E (SingleSVarExpr sv1 e1) :=?: E (SingleSVarExpr sv2 e2))
                | sv1 == sv2    = case (null e1, null e2) of
                    (True , True ) ->   biset_tautology
                    (False, True ) ->   orientation
                    (True , False) ->   biset_application
                    (False, False) ->   biset_distribution
                | otherwise     = case (null e1, null e2) of
                    (True , True ) ->   biset_application
                    (False, True ) ->   orientation
                    (True , False) ->   biset_application
                    (False, False) ->   biset_distribution
singleTrueSet (E e :=?: E (SingleSVarExpr sv e2)) = orientation
singleTrueSet (E (SingleSVarExpr sv e1) :=?: E e)
    | null e1   = set_application
    | otherwise = set_distribution
singleTrueSet (V v1 :=?: V v2)
    | v1 == v2  = tautology
    | otherwise = case (isMeta v1, isMeta v2) of
        (False, False) -> clash
        (True , False) -> application
        (False, True ) -> orientation
        (True , True ) -> application
