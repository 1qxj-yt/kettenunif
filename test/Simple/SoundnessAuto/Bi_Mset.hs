module Simple.SoundnessAuto.Bi_Mset
    ( isSound
    ) where

import Test.QuickCheck
import Data.Char(isUpper)
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import qualified Data.List(foldl')

import Simple.Substitution(identity,compose,onAny)
import Simple.Expression
import Simple.UnifProblem
import Simple.Algorithm(solve)

instance Arbitrary Var where
    arbitrary = do
        --c <- elements "abcdpqrstuvwxyzABCDVWXYZ"
        c <- elements "abcxyzABXY"
        return $ (if isUpper c then meta else var) c 0

instance Arbitrary Bind where
    arbitrary = do
        v1 <- arbitrary
        v2 <- arbitrary
        return $ v1 := v2

instance Arbitrary SetVar where
    arbitrary = do
        i <- choose (1, 10)
        return $ SetVar i

instance Arbitrary Expr where
    arbitrary = do
        n <- choose (0,5)
        e <- vectorOf n arbitrary
        sv <- arbitrary
        frequency [
            (1, return $ Expr e),
            (3, return $ SingleSVarExpr sv e)
            ]

instance Arbitrary UnifProblemEl where
    arbitrary = do
        e1 <- arbitrary
        e2 <- arbitrary
        return $ e1 :=.: e2

--solvesInSecs s σ p = null σ `trivial` within (inSecs 1) (solves σ p)

isSound :: UnifProblem -> Bool
isSound p = and [ σ `solves` p | σ <- solve p ]

inSecs = (1000000*)