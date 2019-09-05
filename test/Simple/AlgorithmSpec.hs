module Simple.AlgorithmSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Simple.Algorithm

import Simple.SoundnessAuto.Bi_Mset
import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Bind((:=))
    , SetVar(SetVar)
    , Var, var, meta
    , Token(..)
    , addApos
    , expr
    , ssve
    )
import Simple.UnifProblem
    ( UnifProblem
    , UnifProblemEl((:=.:))
    )
import Simple.Substitution
    ( Substitution
    , (→)
    , (→→)
    , build
    , equivalent
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
    describe "QuickCheck" $ do
        modifyMaxSize (const 10) $ modifyMaxSuccess (const 100) $
            prop "solves 100 test cases for bi-mset (Hspec)" $
                isSound
    describe "solve" $ do
        it "solves {x=Y =. X=y} to [{X→x,Y→y}]" $ do
            solve testProblem1 `shouldBe` [build [v 'X' → v 'x', v 'Y' → v 'y']]
        it "solves {x=x =. z=z} to []" $ do
            solve testProblem2 `shouldBe` []
        it "solves {X=Y =. Y=a} to [{X→a,Y→a}]" $ do
            solve testProblem3 `shouldBe` [build [v 'X' → v 'a', v 'Y' → v 'a']]
        it "solves {X=Y =. Y=A} to [{X→A,Y→A}]" $ do
            solve testProblem4 `shouldSatisfy` and . zipWith equivalent [build [v 'X' → v 'A', v 'Y' → v 'A']]
        it "solves {X=Y =. Y=A} to ~[{A→X,Y→X}]" $ do
            solve testProblem4 `shouldSatisfy` and . zipWith equivalent [build [v 'A' → v 'X', v 'Y' → v 'X']]
        it "solves {[A=B,C=D] =. [x=y,z=w]} to [{A→x,B→y,C→z,D→w},{A→z,B→w,C→x,D→y}]" $ do
            solve testProblem6 `shouldBe` [ build [v 'A'→ v 'x', v 'B' → v 'y', v 'C' → v 'z', v 'D' → v 'w'],
                                            build [v 'A'→ v 'z', v 'B' → v 'w', v 'C' → v 'x', v 'D' → v 'y'] ]
    describe "solve with set" $ do
        it "solves {M:[X=a] =. [A=a,B=D]} to [{M→[B=D]|X→A},{M→[A=a]|D→a,X→B}]" $ do
            solve testProblem5 `shouldBe` [ build [SetVar 0 →→ expr [v 'B' := v 'D'], v 'X' → v 'A'],
                                            build [SetVar 0 →→ expr [v 'A' := v 'a'], v 'D' → v 'a', v 'X' → v 'B'] ]



------------------------------------------------
-- Test Data
------------------------------------------------

testProblem1 :: UnifProblem
testProblem1 = S.fromList [ expr [v 'X' := v 'y'] :=.: expr [v 'x' := v 'Y']]

testProblem2 :: UnifProblem
testProblem2 = S.fromList [ expr [v 'x' := v 'x'] :=.: expr [v 'y' := v 'y']]

testProblem3 :: UnifProblem
testProblem3 = S.fromList [ expr [v 'X' := v 'Y'] :=.: expr [v 'Y' := v 'a']]

testProblem4 :: UnifProblem
testProblem4 = S.fromList [ expr [v 'X' := v 'Y'] :=.: expr [v 'Y' := v 'A']]

testProblem5 :: UnifProblem
testProblem5 = S.fromList [ ssve (SetVar 0) [v 'X' := v 'a'] :=.: expr [v 'A' := v 'a', v 'B' := v 'D'] ]

testProblem6 :: UnifProblem
testProblem6 = S.singleton $ expr [v 'A' := v 'B', v 'C' := v 'D'] :=.: expr [v 'x' := v 'y', v 'z' := v 'w']

testProblem7 :: UnifProblem
testProblem7 = S.singleton $ ssve (SetVar 0) [v 'X' := v 'a']
                            :=.: ssve (SetVar 0) [v 'c' := v 'd', v 'x' := v 'A']
