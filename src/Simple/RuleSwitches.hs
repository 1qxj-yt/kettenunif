module Simple.RuleSwitches
    ( fullMSet
    ) where

import Simple.Rules

import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Token(E,B,V)
    , isMeta
    , eNull
    , eNullS
    , disjointS
    , eLengthS
    )
import Simple.UnifProblem
    ( Equation((:=?:))
    , isBlockEq
    )



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
fullMSet eq@(E e1 :=?: E e2)
    | not (disjointS e1 e2) = x_semi_tautology
    | otherwise             = case (eNull e1, eNull e2) of
                    (True , True ) -- -> x_emp_application
                                | eNullS e1 && eNullS e2 -> tautology
                                | isBlockEq eq -> x_rep_application
                                | eLengthS e1 == 1 -> x_app_accelerationL
                                | otherwise -> x_emp_application
                    (False, True ) | isBlockEq eq -> x_rep_application
                                   | eLengthS e2 == 1 -> x_app_accelerationR
                                   | otherwise -> orientation
                    (True , False) | eNullS e1 -> clash
                                -- x_application -- x_part_rapp
                                   | isBlockEq eq -> x_rep_application
                                   | eLengthS e1 == 1 -> x_app_accelerationL
                                   | otherwise -> x_partition
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
