module Simple.Substitution
    ( Substitution
    -- * Construction
    , (→)
    , (→→)
    , identity
    , build
    -- * Operations
    , compose
    , equivalent
    , restrict
    -- * Checks
    , isValid
    -- * Application
    , onAny
    , onExpr
    ) where

import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Bind((:=))
    , Var
    , SetVar
    , Token(E,B,V)
    , isMeta
    )
import qualified Simple.VarSubst as Var
import qualified Simple.SetSubst as Set

import Data.List(find,nub,intercalate,groupBy)
import qualified Data.Map as M
import qualified Data.Set as S

------------------------------------------------
-- Data Types
------------------------------------------------

data Substitution = Subst { setComponent :: Set.Substitution
                          , varComponent :: Var.Substitution } deriving (Eq,Ord)

instance Show Substitution where
    show (Subst setC varC)
        | Set.isIdentity setC && Var.isIdentity varC = "id"
        | otherwise = '{': (show setC) ++ ('|': (show varC) ++ "}")

instance Monoid Substitution where
    mempty = identity
    mappend = compose

-- Constructor
-- | Single variable mapping.
infixl →
(→) :: Var -> Var -> Substitution
v1 → v2 = if isMeta v1
    then Subst Set.identity (v1 Var.→ v2)
    else error "substitution origin is non-meta"

-- | Single set mapping.
infixl →→
(→→) :: SetVar -> Expr -> Substitution
sv →→ e  = Subst (sv Set.→ e) Var.identity

identity :: Substitution
identity = Subst Set.identity Var.identity

isValid :: Substitution -> Bool
isValid (Subst setC varC) = Var.isValid varC

restrict :: Substitution -> Substitution
restrict (Subst setC varC) = Subst (Set.restrict setC) varC

------------------------------------------------
-- Operations
------------------------------------------------

extend :: Substitution -> Substitution -> Substitution
extend (Subst sL vL) (Subst sR vR) = Subst (Set.extend sL sR) (Var.extend vL vR)

-- | Constructs a substitution from a list of sub-substitutions.
-- Throws an error if contradictory entries are found.
build :: [Substitution] -> Substitution
build = foldr extend identity

-- | Constructs a substitution whose application is equivalent to
-- applying the right substitution first, then the left substitution.
-- Throws an error if contradictory entries are found.
compose :: Substitution -> Substitution -> Substitution
compose (Subst sL vL) (Subst sR vR) =
        let varC = Var.compose vL vR
            newr = Set.mapOnImage (vL `Var.onExpr`) sR
            setC = Set.compose sL newr
        in  Subst setC varC

equivalent :: Substitution -> Substitution -> Bool
equivalent σ1 σ2 = Var.equivalent (varComponent σ1) (varComponent σ2)


------------------------------------------------
-- Substitution Application
------------------------------------------------

onVar :: Substitution -> (Var -> Var)
onVar (Subst s v) = Var.onVar v

onBind :: Substitution -> (Bind -> Bind)
onBind σ (v1:=v2) = σ `onVar` v1 := (σ `onVar` v2)

onExpr :: Substitution -> (Expr -> Expr)
onExpr (Subst s v) expr = s `Set.onExpr` (v `Var.onExpr` expr)

onAny :: Substitution -> Token -> Token
onAny σ t = case t of
                E e -> E (σ `onExpr` e)
                B b -> B (σ `onBind` b)
                V v -> V (σ `onVar`  v)
