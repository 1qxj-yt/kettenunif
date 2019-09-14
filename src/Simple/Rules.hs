module Simple.Rules
    ( Rule(name,apply)
    , Input
    , Output
    , SSList(SSL)
    -- * Rules
    , termination
    , tautology
    , clash
    , distribution
    , decomposition
    , application
    , orientation
    , set_distribution
    , set_application
    , biset_tautology
    , biset_application
    , biset_distribution
    , mset_semi_tautology
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
    , onAny
    , compose
    , identity
    )
import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Bind((:=))
    , Token(E,B,V)
    , foldWithIndex
    , eHead
    , eTail
    , eDelete
    , clean
    , addApos
    )
import Simple.Binds
    ( cons
    , uncons
    , deleteAt
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

-- | Same as tautology; just for logging the termination.
termination :: Rule
termination = R "termination" (\(sol,_,γ) -> [(sol,γ)])

tautology :: Rule
tautology = R "tautology" (\(sol,_,γ) -> [(sol,γ)])

clash :: Rule
clash = R "clash" (const [])

distribution :: Rule
distribution = R "distribution" (\(sol,E e1 :=?: E e2,γ) -> foldWithIndex (\i b2 ->
                        [(sol, (B (eHead e1) :=?: B b2) % (E (eTail e1) :=?: E (eDelete i e2)) % γ)]
                    ) e2)

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

x_semi_tautology :: Rule
x_semi_tautology = R "x-semi-tautology" (\(sol, E e1 :=?: E e2, γ) ->
        let (e1',e2') = clean e1 e2
        in  [(sol, (E e1' :=?: E e2') % γ)] )

-- | Note: Includes set_clash
set_distribution :: Rule
set_distribution = R "set-distribution" (apply distribution)

set_application :: Rule
set_application = R "set-application" (\(SSL sol,E (SingleSVarExpr sv _) :=?: E e,γ) ->
            [(SSL ((sv →→ e):sol), (sv →→ e) `onSolver` γ)] )

set_orientation :: Rule
set_orientation = R "set-orientation" (apply orientation)

biset_tautology :: Rule
biset_tautology = R "biset-tautology" (\(sol,E (SingleSVarExpr _ e1) :=?: E (SingleSVarExpr _ e2),γ) ->
            [(sol, (E (Expr e1) :=?: E (Expr e2)) % γ)] )

biset_distribution :: Rule
biset_distribution = R "biset-distribution" (\(SSL sol,E (SingleSVarExpr sv1 e1) :=?: E e2sv@(SingleSVarExpr sv2 e2),γ) ->
            let sv2' = addApos sv2
                sv1' = if sv1 == sv2 then sv2' else sv1
                (b1,e1s) = uncons e1
                b1s = (cons b1 mempty)
            in  ((SSL ((sv2 →→ SingleSVarExpr sv2' b1s):sol)),
                    (E (SingleSVarExpr sv1' e1s) :=?: E (SingleSVarExpr sv2' e2)) % ((sv2 →→ SingleSVarExpr sv2' b1s) `onSolver` γ) ):
                        (foldWithIndex (\i b2 ->
                            [(SSL sol, (B b1 :=?: B b2) % (E (SingleSVarExpr sv1 e1s) :=?: E (eDelete i e2sv)) % γ)]
                        )) e2sv
            )

biset_application :: Rule
biset_application = R "biset-application" (\(SSL sol,E (SingleSVarExpr sv _) :=?: E e,γ) ->
                [(SSL ((sv →→ e):sol), (sv →→ e) `onSolver` γ)] )

mset_semi_tautology :: Rule
mset_semi_tautology = R "mset-semi-tautology" (\(sol, E (SingleSVarExpr _ e1) :=?: E (SingleSVarExpr _ e2), γ) ->
            [(sol, (E (Expr e1) :=?: E (Expr e2)) % γ)])
