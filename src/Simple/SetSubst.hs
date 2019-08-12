module Simple.SetSubst
    ( Substitution
    -- * Construction
    , (→)
    , identity
    -- * Operations
    , extend
    , compose
    -- * Checks
    , isIdentity
    -- * Application
    , onExpr
    , mapOnImage
    ) where

import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Bind
    , Var
    , SetVar
    )

import Data.List(intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

------------------------------------------------
-- Data Types
------------------------------------------------

data Substitution = Subst {mp :: M.Map SetVar Expr} deriving Eq

instance Show Substitution where
    show (Subst mp)
        | M.null mp = "id"
        | otherwise = intercalate "," (map showAsc (M.assocs mp))
            where showAsc (sv,expr) = show sv++"→"++(show expr)

-- Constructor
-- | Single mapping.
infixl →
(→) :: SetVar -> Expr -> Substitution
sv → e = Subst (M.singleton sv e)

identity :: Substitution
identity = Subst M.empty

isIdentity :: Substitution -> Bool
isIdentity σ = M.null (mp σ)


------------------------------------------------
-- Operations
------------------------------------------------

extend :: Substitution -> Substitution -> Substitution
extend sl sr = Subst $ M.unionWith sound (mp sl) (mp sr)
    where sound a1 a2 = if a1==a2 then a1 else error "contradictory entries"

-- | Constructs a substitution from a list of sub-substitutions.
-- Throws an error if contradictory entries are found.
build :: [Substitution] -> Substitution
build = foldr extend (Subst M.empty)

compose :: Substitution -> Substitution -> Substitution
compose sl sr =
        let newr = Subst $ M.map (sl `onExpr`) (mp sr)
        in  extend sl newr

------------------------------------------------
-- Substitution Application
------------------------------------------------

onExpr :: Substitution -> (Expr -> Expr)
onExpr σ ssve@(SingleSVarExpr sv e) = case M.lookup sv (mp σ) of
    Nothing -> ssve
    Just (Expr e2) -> Expr (e2++e)
onExpr σ ssve = ssve

mapOnImage :: (Expr -> Expr) -> Substitution -> Substitution
mapOnImage f (Subst mp) = Subst (M.map f mp)
