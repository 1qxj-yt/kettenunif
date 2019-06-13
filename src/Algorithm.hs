module Algorithm where

import Unification
    ( SolverDS
    , Equation
    )
import Substitution
    ( Substitution
    )


------------------------------------------------
-- Data Types
------------------------------------------------

type Input  = (Substitution, Equation, SolverDS)
type Output = (Substitution, SolverDS)
type Rule = Input -> [Output]




solve = undefined