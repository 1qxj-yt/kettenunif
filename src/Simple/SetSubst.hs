module Simple.SetSubst
    ( Substitution
    -- * Construction
    , (→)
    , identity
    -- * Operations
    , extend
    , compose
    , restrict
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
    , SetVar(SetVar)
    , setExpr
    , foldWithIndex
    , foldWithIndexSet
    )

import Simple.Binds as B
        ( head )

import Data.List(intercalate, nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid(Any(Any),getAny)

------------------------------------------------
-- Data Types
------------------------------------------------

data Substitution = Subst {mp :: M.Map SetVar Expr} deriving (Eq,Ord)

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
    where sound a1 a2 = if a1==a2 then a1
                else error $ "contradictory entries: " ++ show a1 ++ " and "++ show a2

-- | Constructs a substitution from a list of sub-substitutions.
-- Throws an error if contradictory entries are found.
build :: [Substitution] -> Substitution
build = foldr extend (Subst M.empty)

compose :: Substitution -> Substitution -> Substitution
compose sl sr = cleanUp $ Subst $ M.union (M.map (sl `onExpr`) (mp sr)) (mp sl)

-- | Removes entries of the form \(M\mapsto M':[]\) with \(M==M'\)
cleanUp :: Substitution -> Substitution
cleanUp (Subst s) = Subst (M.filterWithKey neq s)
    where   m `neq` SingleSVarExpr m' e = not (null e) || m /= m'
            n `neq` (Expr _) = True
            m `neq` se =
                getAny (foldWithIndex (\i b -> Any True) se) -- not (null e)
                ||
                getAny (foldWithIndexSet (\i m' -> Any (i >= 1 || m /= m')) se)
                -- if length m' < 1 then True else m /= (B.head m')

-- | Restricts substitution to non-helper variables.
restrict :: Substitution -> Substitution
restrict (Subst s) = Subst (M.filterWithKey (\k _ -> isNotHelper k) s)
    where   isNotHelper (SetVar i)  = True
            isNotHelper _           = False

------------------------------------------------
-- Substitution Application
------------------------------------------------

onExpr :: Substitution -> (Expr -> Expr)
onExpr σ ssve@(SingleSVarExpr sv e) = case M.lookup sv (mp σ) of
    Nothing -> ssve
    Just (Expr e2) -> Expr (e2 `mappend` e)
    Just (SingleSVarExpr m e2) -> SingleSVarExpr m (e2 `mappend` e)
onExpr σ ssve@(Expr _) = ssve
onExpr σ se = setExpr [] (foldWithIndex (\_ b -> [b]) se)
    `mappend` foldWithIndexSet (\_ sv -> case M.lookup sv (mp σ) of
        Nothing -> setExpr [sv] []
        Just e' -> e'
    ) se

mapOnImage :: (Expr -> Expr) -> Substitution -> Substitution
mapOnImage f (Subst mp) = Subst (M.map f mp)
