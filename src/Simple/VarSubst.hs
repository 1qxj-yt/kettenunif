module Simple.VarSubst
    ( Substitution
    -- * Construction
    , (→)
    , identity
    -- * Operations
    , extend
    , compose
    , build
    -- * Checks
    , isIdentity
    , isValid
    , equivalent
    -- * Application
    , onVar
    , onBind
    , onExpr
    ) where

import Simple.Expression
    ( Expr
    , Bind
    , Var
    , Token(E,B,V)
    , isMeta
    , ωBind
    , ωExpr
    )

import Data.List(intercalate)
import qualified Data.Map as M
import qualified Data.Set as S


------------------------------------------------
-- Data Types
------------------------------------------------

data Substitution = Subst {mp :: M.Map Var Var} deriving (Eq,Ord)

instance Show Substitution where
    show (Subst mp)
        | M.null mp = "id"
        | otherwise = intercalate "," (map showAsc (M.assocs mp))
            where showAsc (v1,v2) = show v1++"→"++(show v2)

-- Constructor
-- | Single mapping.
infixl →
(→) :: Var -> Var -> Substitution
v1 → v2 = if isMeta v1
    then Subst $ M.singleton v1 v2
    else error "substitution origin is non-meta"

identity :: Substitution
identity = Subst M.empty

isIdentity :: Substitution -> Bool
isIdentity σ = M.null (mp σ)

isValid :: Substitution -> Bool
isValid σ = let origins = M.keys (mp σ)
                    in  all isMeta origins


------------------------------------------------
-- Operations
------------------------------------------------

extend :: Substitution -> Substitution -> Substitution
extend sl sr = Subst $ M.unionWith sound (mp sl) (mp sr)
    where sound a1 a2 = if a1==a2 then a1 else error "contradictory entries"

-- | Constructs a substitution from a list of sub-substitutions.
-- Throws an error if contradictory entries are found.
build :: [Substitution] -> Substitution
build = foldr extend (Subst $ M.empty)

-- | Constructs a substitution whose application is equivalent to
-- applying the right substitution first, then the left substitution.
-- Throws an error if contradictory entries are found.
--
-- > (B→C) `compose` (A→B) == (A→C,B→C)
-- > (X→a) `compose` (Y→b) == (X→a,Y→b)
-- > (X→a) `compose` (X→b) == (X→b)
-- > (X→a) `compose` (X→a) == (X→a)
compose :: Substitution -> Substitution -> Substitution
compose sl sr = cleanUp $ Subst $ M.union (M.map (sl `onVar`) (mp sr)) (mp sl)

equivalent :: Substitution -> Substitution -> Bool
equivalent σ1 σ2 =
        let (ltoMeta, ltoVar) = M.partition isMeta (mp σ1)
            (rtoMeta, rtoVar) = M.partition isMeta (mp σ2)
        in  case findEquatingPerm (Subst ltoMeta) (Subst rtoMeta) of
                Just _  -> ltoVar == rtoVar
                Nothing -> False

-- Returns a permutation \(\pi\) such that \(\pi\circ\sigma_1=\sigma_2\),
-- if such a substitution exisists.
findEquatingPerm :: Substitution -> Substitution -> Maybe Substitution
findEquatingPerm σ1 σ2 =
        let codomain = S.toList $ M.keysSet (mp σ1) `S.union` M.keysSet (mp σ2)
            σ1onCod  = map (σ1 `onVar`) codomain
            σ2onCod  = map (σ2 `onVar`) codomain
            potentialAssocs = zip σ1onCod σ2onCod
        in  findEquatingPermAux potentialAssocs

-- Constructs and returns the corresponding substitution,
-- if the passed list describe the assocs of a permutation.
-- Assuming all variables of the list to be meta.
findEquatingPermAux :: [(Var,Var)] -> Maybe Substitution
findEquatingPermAux = (Subst <$>) . sequence
            .   M.fromListWith (\a1 a2 -> if a1 == a2 then a1 else Nothing)
            .   (map (\(k,a) -> (k,Just a)))

-- | Removes entries of the form \(x\mapsto y\) with \(x==y\)
cleanUp :: Substitution -> Substitution
cleanUp (Subst s) = Subst (M.filterWithKey (/=) s)

------------------------------------------------
-- Substitution Application
------------------------------------------------

onVar :: Substitution -> (Var -> Var)
onVar σ v1 = case M.lookup v1 (mp σ) of
    Nothing -> v1
    Just v2 -> v2

onBind :: Substitution -> (Bind -> Bind)
onBind σ = ωBind (onVar σ)

onExpr :: Substitution -> (Expr -> Expr)
onExpr σ = ωExpr (onVar σ)

onAny :: Substitution -> Token -> Token
onAny σ t = case t of
                E e -> E (σ `onExpr` e)
                B b -> B (σ `onBind` b)
                V v -> V (σ `onVar`  v)
