module Simple.Substitution
    ( Substitution
    -- * Construction
    , (→)
    , identity
    , build
    -- * Operations
    , compose
    , equivalent
    -- * Checks
    , isValid
    -- * Application
    , onAny
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
                          , varComponent :: Var.Substitution } deriving Eq

instance Show Substitution where
    show (Subst setC varC)
        | Set.isIdentity setC && Var.isIdentity varC = "id"
        | otherwise = '{': (show setC) ++ ('|': (show varC) ++ "}")

-- Constructor
-- | Single variable mapping.
infixl →
(→) :: Var -> Var -> Substitution
v1 → v2 = if isMeta v1
    then Subst Set.identity (v1 Var.→ v2)
    else error "substitution origin is non-meta"

identity :: Substitution
identity = Subst Set.identity Var.identity

isValid :: Substitution -> Bool
isValid (Subst setC varC) = Var.isValid varC

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
            setC = Set.compose sL sR
        in  Subst (Set.mapOnImage (varC `Var.onExpr`) setC) varC

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
onExpr (Subst s v) expr = v `Var.onExpr` (s `Set.onExpr` expr)

onAny :: Substitution -> Token -> Token
onAny σ t = case t of
                E e -> E (σ `onExpr` e)
                B b -> B (σ `onBind` b)
                V v -> V (σ `onVar`  v)
