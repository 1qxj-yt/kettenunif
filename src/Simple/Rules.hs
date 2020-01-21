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
    -- * Set
    , x_semi_tautology
    , x_distribution
    , x_emp_application
    , x_partition
    , x_rep_application
    , x_app_accelerationL
    , x_app_accelerationR
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
    , build
    , identity
    )
import Simple.Expression
    ( Expr(SetExpr)
    , Bind((:=))
    , Token(E,B,V)
    , setExpr
    , foldWithIndex
    , foldWithIndexSet
    , eConsS
    , eLength
    , eHead
    , eHeadS
    , eTail
    , eDelete
    , eDeleteS
    , clean
    , combine
    , addApos
    , decompose
    , partition
    , sgSplit
    , prepareRec
    , stopRec
    )
import Simple.Binds
    ( cons
    , uncons
    , dPart
    , fromList
    )

import qualified Data.Set as S
import Data.List(delete)
import Data.Foldable(toList)

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

x_distribution :: Rule
x_distribution = R "x-distribution" $ (\(SSL sol, E e1 :=?: E e2, γ) ->
                    let b1 = eHead e1 in
                    foldWithIndex (\i b2 ->
                        [(SSL sol, (B b1 :=?: B b2) % (E (eTail e1) :=?: E (eDelete i e2)) % γ)]
                    ) e2
                    `mappend`
                    foldWithIndexSet (\i ni ->
                        let ni' = addApos ni
                            τ = (ni →→ setExpr [ni'] [b1])
                        in  [(SSL (τ:sol), τ `onSolver` ((E (eTail e1) :=?: E (eConsS ni' $ eDeleteS i e2)) % γ))]
                    ) e2
                    )

x_partition :: Rule
x_partition = R "x-partition" (\(SSL sol, E e1 :=?: E e2, γ) ->
    let (ns,e) = decompose e2
        (ms,_) = decompose e1
        mss    = partition ms
        (ms',ns') = unzip $ map (\(m,_,n) -> (m,n)) mss
        prepRec m = prepareRec m (setExpr [combine m n' | n' <- toList ns ] [])
        τ = build $
            [ m →→ setExpr [prepRec m] [] | m <- ms' ]
             ++ [ n' →→ setExpr (grN:[combine m n' | m <- toList ms ]) []
                    | n' <- toList ns, let (_stN,grN) = sgSplit n' ]
    in  [ (SSL (τ:sol),
                 foldr (%)
                     ( -- (E (SetExpr (fmap waitBase ms) mempty) :=?:
                            -- E (SetExpr (fmap (fst.sgSplit) ns) mempty)) % -- rem
                       (E (SetExpr (fmap (snd.sgSplit) ns) mempty) :=?:
                            E (setExpr ns' []) ) %
                        (τ `onSolver` γ))
                     [ E (setExpr (replicate c (prepRec m)) [])
                                 :=?: E (SetExpr (fromList [n']) (ζ m))
                         | (m,c,n') <- mss ] )
            | ζ <- dPart (fromList ms') e ])

x_rep_application :: Rule
x_rep_application = R "x-rep-application" (\(SSL sol, E e1 :=?: E e2, γ) ->
    let (ms,e) = decompose e1
        mss    = partition ms
        [(m,c,_)] = mss
    in  case eLength e2 of
        0 ->
            let τm = (m →→ stopRec m)
                n = eHeadS e2
                τn = (n →→ SetExpr mempty e)
            in  [(SSL (τm:τn:sol), τm `onSolver` (τn `onSolver` γ) )]
        _ ->
            let m' = addApos m
                b1 = eHead e2
                -- τ = (m →→ setExpr (if eLength e2 == 1 then [] else [m']) [b1])
                τ = (m →→ setExpr [m'] [b1])
            in  [ (SSL (τ:sol),
                        (E (setExpr (replicate c m') (replicate (c-1) b1))
                            :=?: E (eTail e2)) % (τ `onSolver` γ)
                    )]
        )

x_app_accelerationL :: Rule
x_app_accelerationL = R "x-app-accelerationL" (\(SSL sol, E e1 :=?: E e2, γ) ->
    let (ns,_) = decompose e1
        m = head (toList ns)
        τ = (m →→ e2)
    in  [ (SSL (τ:sol), τ `onSolver` γ) ])

x_app_accelerationR :: Rule
x_app_accelerationR = R "x-app-accelerationR" (\(SSL sol, E e1 :=?: E e2, γ) ->
    let (ns,_) = decompose e2
        m = head (toList ns)
        τ = (m →→ e1)
    in  [ (SSL (τ:sol), τ `onSolver` γ) ])



x_emp_application :: Rule
x_emp_application = R "x-emp-application" (\(SSL sol, E e1 :=?: E e2, γ) -> [
        let μ  = foldWithIndexSet (\_ mi ->
                    (mi →→ ( setExpr
                        (foldWithIndexSet (\_ ni ->
                            [combine mi ni]
                        ) e2) [] )
                    ) ) e1
            ν  = foldWithIndexSet (\_ ni ->
                    (ni →→ ( setExpr
                        (foldWithIndexSet (\_ mi ->
                            [combine mi ni]
                        ) e1) [] )
                    ) ) e2
        in  (SSL (μ:ν:sol),
                ((μ `mappend` ν) `onSolver` γ) ) ] )
