module Simple.RuleSwitches
    ( fullMSet
    ) where

import Simple.Rules

import Simple.Expression
    ( Expr
    , Token(E,B,V)
    , isMeta
    , eNull
    , eNullS
    , disjointS
    , eLength
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
                    (True , True )
                                | eNullS e1 && eNullS e2 -> tautology
                                | isBlockEq eq -> x_rep_application
                                | eLengthS e1 == 1 -> x_app_accelerationL
                                | otherwise -> x_emp_application
                    (False, True ) | isBlockEq eq -> x_rep_application
                                   | eLengthS e2 == 1 -> x_app_accelerationR
                                   | otherwise -> orientation
                    (True , False) | eNullS e1 -> clash
                                   | isBlockEq eq -> x_rep_application
                                   | eLengthS e1 == 1 -> x_app_accelerationL
                                   | otherwise -> x_partition
                    (False, False) | eNullS e1 && eNullS e2 && eLength e1 /= eLength e2 -> clash
                                   | otherwise -> x_distribution
