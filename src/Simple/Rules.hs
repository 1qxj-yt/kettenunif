module Simple.Rules
    ( Rule(name,apply)
    , Input
    , Output
    , SSList(SSL)
    -- * Rules
    , tautology
    , clash
    , distribution
    , decomposition
    , application
    , orientation
    , set_distribution
    , set_application
    ) where

import Simple.UnifProblem
    ( SolverDS
    , Equation((:=?:))
    , onSolver
    )
import Simple.Substitution
    ( Substitution
    , (→)
    , (→→)
    )
import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Bind((:=))
    , Token(E,B,V)
    )

import qualified Data.Set as S
import Data.List(delete)

------------------------------------------------
-- Data Types
------------------------------------------------
-- | Just a list of substitutions,
-- but meant to (type-)distinguish
-- a list of sub-substitutions (especially single mappings)
-- that will be composed to a whole solution later
-- from a list of valid solution substitutions.
--
-- In the context of that motivation,
-- @SSList@ is just a different representation of a whole @Substitution@.
newtype SSList = SSL [Substitution]
type Input  = (SSList, Equation, SolverDS)
type Output = (SSList, SolverDS)
data Rule   = R {name::String, apply::Input -> [Output]}

instance Show Rule where
    show = name

instance Show SSList where
    show (SSL list) = show list


infixr %
(%) :: Equation -> SolverDS -> SolverDS
(%) = S.insert


tautology :: Rule
tautology = R "tautology" (\(sol,_,γ) -> [(sol,γ)])

clash :: Rule
clash = R "clash" (const [])

distribution :: Rule
distribution = R "distribution" (\(sol,E (Expr (b1:e1s)) :=?: E (Expr e2),γ) ->
            [(sol, (B b1 :=?: B b2) % (E (Expr e1s) :=?: E (Expr $ delete b2 e2)) % γ) | b2<-e2 ] )

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


------------------------------------------------
-- Set Extension
------------------------------------------------

-- | Note: Includes set_clash
set_distribution :: Rule
set_distribution = R "set-distribution" (\(sol,E (SingleSVarExpr sv (b1:e1s)) :=?: E (Expr e2),γ) ->
            [(sol, (B b1 :=?: B b2) % (E (SingleSVarExpr sv e1s) :=?: E (Expr $ delete b2 e2)) % γ) | b2 <- e2])

set_application :: Rule
set_application = R "set-application" (\(SSL sol,E (SingleSVarExpr sv []) :=?: E e,γ) ->
            [(SSL ((sv →→ e):sol), (sv →→ e) `onSolver` γ)] )

-- 'set_orientation' is covered by 'orientation'.

biset_tautology :: Rule
biset_tautology = R "biset-tautology" (\(sol,E (SingleSVarExpr _ e1) :=?: E (SingleSVarExpr _ e2),γ) ->
            [(sol, (E (Expr e1) :=?: E (Expr e2)) % γ)] )

biset_distribution :: Rule
biset_distribution = R "biset-distribution" (\(SSL sol,E (SingleSVarExpr sv1 (b1:e1s)) :=?: E (SingleSVarExpr sv2 e2),γ) ->
            ((SSL ((sv2 →→ SingleSVarExpr sv2 [b1]):sol)), (E (SingleSVarExpr sv1 e1s) :=?: E (SingleSVarExpr sv2 e2)) % γ):
            [(SSL sol, (B b1 :=?: B b2) % (E (SingleSVarExpr sv1 e1s) :=?: E (SingleSVarExpr sv2 $ delete b2 e2)) % γ) | b2 <- e2])

biset_application :: Rule
biset_application = R "biset-application" (\(SSL sol,E (SingleSVarExpr sv []) :=?: E e,γ) ->
            [(SSL ((sv →→ e):sol), (sv →→ e) `onSolver` γ)])
