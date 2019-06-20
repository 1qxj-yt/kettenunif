module Substitution
    ( Substitution
    , (→)
    , identity
    , build
    , compose
    , equivalent
    , isValid
    , onAny
    ) where

import Expression
    ( Expr
    , Bind((:=))
    , Var
    , Token(E,B,V)
    , isMeta
    )

import Data.List(find,nub,intercalate,groupBy)
import qualified Data.Map as M
import qualified Data.Set as S

------------------------------------------------
-- Data Types
------------------------------------------------

data Substitution = Subst {mp :: M.Map Var Var} deriving Eq

instance Show Substitution where
    show (Subst mp) = '{': (intercalate "," $ map showAsc (M.assocs mp)) ++ "}"
        where showAsc (v1,v2) = show v1++"→"++(show v2)

-- Constructor
infixl →
(→) :: Var -> Var -> Substitution
v1 → v2 = if isMeta v1
    then Subst (M.singleton v1 v2)
    else error "substitution origin is non-meta"

identity :: Substitution
identity = Subst M.empty -- == build []

isValid :: Substitution -> Bool
isValid σ = let origins = M.keys (mp σ)
              in  all isMeta origins

------------------------------------------------
-- Operations
------------------------------------------------

extend :: Substitution -> Substitution -> Substitution
extend sl sr = Subst $ M.unionWith sound (mp sl) (mp sr)
    where sound a1 a2 = if a1==a2 then a1 else error "contradictory entries"

build :: [Substitution] -> Substitution
build = foldr extend (Subst $ M.empty)

compose :: Substitution -> Substitution -> Substitution
compose sl sr =
    let newr = Subst $ M.map (sl `onVar`) (mp sr)
    in  extend sl newr

equivalent :: Substitution -> Substitution -> Bool
equivalent σ1 σ2 =
        let (ltoMeta, ltoVar) = M.partition isMeta (mp σ1)
            (rtoMeta, rtoVar) = M.partition isMeta (mp σ2)
        in  case findEquatingPerm (Subst ltoMeta) (Subst rtoMeta) of
                Just _  -> ltoVar == rtoVar
                Nothing -> False

findEquatingPerm :: Substitution -> Substitution -> Maybe Substitution
findEquatingPerm σ1 σ2 =
        let codomain = S.toList $ M.keysSet (mp σ1) `S.union` M.keysSet (mp σ2)
            σ1onCod  = map (σ1 `onVar`) codomain
            σ2onCod  = map (σ2 `onVar`) codomain
            potentialAssocs = zip σ1onCod σ2onCod
        in  findEquatingPermAux potentialAssocs

-- Assuming variables to be meta
findEquatingPermAux :: [(Var,Var)] -> Maybe Substitution
findEquatingPermAux = (Subst <$>) . sequence
            .   M.fromListWith (\a1 a2 -> if a1 == a2 then a1 else Nothing)
            .   (map (\(k,a) -> (k,Just a)))


------------------------------------------------
-- Substitution Application
------------------------------------------------

onVar :: Substitution -> (Var -> Var)
onVar σ v1 = case M.lookup v1 (mp σ) of
    Nothing -> v1
    Just v2 -> v2

onBind :: Substitution -> (Bind -> Bind)
onBind σ (v1:=v2) = σ `onVar` v1 := (σ `onVar` v2)

onExpr :: Substitution -> (Expr -> Expr)
onExpr σ = map (σ `onBind`)

onAny :: Substitution -> Token -> Token
onAny σ t = case t of
                E e -> E (σ `onExpr` e)
                B b -> B (σ `onBind` b)
                V v -> V (σ `onVar`  v)
