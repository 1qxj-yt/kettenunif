module AlgorithmSpec(spec) where

import Test.Hspec
import Algorithm

import Expression
    ( Expr
    , Bind((:=))
    , Var, var, meta
    , Token(..)
    )
import UnifProblem
    ( UnifProblem
    , UnifProblemEl((:=.:))
    )
import Substitution
    ( Substitution
    , (→)
    , build
    )

import Data.Char(isUpper)
import qualified Data.Set as S

------------------------------------------------
-- Utility Functions
------------------------------------------------

-- Easy creation of variables.
v :: Char -> Var
v c = (if ($c) isUpper then meta else var) c 0


------------------------------------------------
-- Specification
------------------------------------------------

spec :: Spec
spec = do
    describe "solve" $ do
        it "solves {x=Y =. X=y} to [{X→x,Y→y}]" $ do
            solve testProblem1 `shouldBe` [build [v 'X' → v 'x', v 'Y' → v 'y']]
        it "solves {x=x =. z=z} to []" $ do
            solve testProblem2 `shouldBe` []
        it "solves {X=Y =. Y=a} to [{X→a,Y→a}]" $ do
            solve testProblem3 `shouldBe` [build [v 'X' → v 'a', v 'Y' → v 'a']]


------------------------------------------------
-- Test Data
------------------------------------------------

testProblem1 :: UnifProblem
testProblem1 = S.fromList [[v 'X' := v 'y'] :=.: [v 'x' := v 'Y']]

testProblem2 :: UnifProblem
testProblem2 = S.fromList [[v 'x' := v 'x'] :=.: [v 'y' := v 'y']]
