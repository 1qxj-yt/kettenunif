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
