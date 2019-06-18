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


tautology :: Rule
tautology (sol,_,γ) = [(sol,γ)]

clash :: Rule
clash _ = []

distribution :: Rule
distribution (sol,E (b1:e1s) :=?: E e2,γ) =
            [(sol, (B b1 :=?: B b2) % (E e1s :=?: E (delete b2 e2)) % γ) | b2<-e2 ]

decomposition :: Rule
decomposition (sol,B (x:=y) :=?: B (x':=y'),γ) =
            [(sol, (V x :=?: V x') % (V y :=?: V y') % γ)]

application :: Rule
application (sol,V v1:=?: V v2,γ) =
            [((v1 → v2):sol, (v1 → v2) `onSolver` γ)]

orientation :: Rule
orientation (sol, x:=?:y, γ) =
            [(sol, (y:=?:x) % γ)]
