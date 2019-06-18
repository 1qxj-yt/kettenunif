module Rules
    ( Rule
    , tautology
    , clash
    , distribution
    , decomposition
    , application
    , orientation
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
data Rule   = R {name::String, apply::Input -> [Output]}



infixr %
(%) :: Equation -> SolverDS -> SolverDS
(%) = S.insert


tautology :: Rule
tautology = R "tautology" (\(sol,_,γ) -> [(sol,γ)])

clash :: Rule
clash = R "clash" (const [])

distribution :: Rule
distribution = R "distribution" (\(sol,E (b1:e1s) :=?: E e2,γ) ->
            [(sol, (B b1 :=?: B b2) % (E e1s :=?: E (delete b2 e2)) % γ) | b2<-e2 ] )

decomposition :: Rule
decomposition = R "decomposition"
            (\ (sol, B (x:=y) :=?: B (x':=y'), γ) ->
            [(sol, (V x :=?: V x') % (V y :=?: V y') % γ)] )

application :: Rule
application = R "application" (\(SSL sol,V v1:=?: V v2,γ) ->
            [(SSL ((v1 → v2):sol), (v1 → v2) `onSolver` γ)] )

orientation :: Rule
orientation = R "orientation" (\(sol, x:=?:y, γ) ->
            [(sol, (y:=?:x) % γ)] )
