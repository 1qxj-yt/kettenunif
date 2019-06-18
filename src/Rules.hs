module Rules
    ( decomposition
    ) where

import UnifProblem
    ( SolverDS
    , Equation((:=?:))
    , onSolver
    )
import Substitution
    ( Substitution
    , Token(E,B,V)
    , (→)
    )
import Expression
    ( Bind((:=))
    )

import qualified Data.Set as S
import Data.List(delete)

------------------------------------------------
-- Data Types
------------------------------------------------

type Input  = ([Substitution], Equation, SolverDS)
type Output = ([Substitution], SolverDS)
type Rule = Input -> [Output]



infixr %
(%) :: Equation -> SolverDS -> SolverDS
(%) = S.insert


decomposition :: Rule
decomposition (sol,B (x:=y) :=?: B (x':=y'),γ) =
            [(sol, (V x :=?: V x') % (V y :=?: V y') % γ)]
