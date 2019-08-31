module Simple.AlgorithmSpec(spec) where

import Test.Hspec
import Simple.Algorithm

import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Bind((:=))
    , SetVar(SetVar)
    , Var, var, meta
    , Token(..)
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
            solve testProblem5 `shouldBe` [ build [SetVar 0 →→ Expr [v 'B' := v 'D'], v 'X' → v 'A'],
                                            build [SetVar 0 →→ Expr [v 'A' := v 'a'], v 'D' → v 'a', v 'X' → v 'B'] ]
        it "solves {M:[X=a] =. M:[c=d, x=A]} to [{M→M:[c=d]|X→x,A→a},{M→M':[c=d,x=A,X=a]}]" $ do
            S.fromList (solve testProblem7) `shouldBe` S.fromList
                                        [   build [
                                                SetVar 0 →→ SingleSVarExpr (SetVar 0) [v 'c' := v 'd']
                                            ,   v 'X' → v 'x'   , v 'A' → v 'a'
                                            ]
                                        ,   build [
                                                SetVar 0 →→ SingleSVarExpr (addApos $ SetVar 0) [v 'c' := v 'd', v 'x' := v 'A', v 'X' := v 'a']
                                            ]
                                        ]



------------------------------------------------
-- Test Data
------------------------------------------------

testProblem1 :: UnifProblem
testProblem1 = S.fromList [ Expr [v 'X' := v 'y'] :=.: Expr [v 'x' := v 'Y']]

testProblem2 :: UnifProblem
testProblem2 = S.fromList [ Expr [v 'x' := v 'x'] :=.: Expr [v 'y' := v 'y']]

testProblem3 :: UnifProblem
testProblem3 = S.fromList [ Expr [v 'X' := v 'Y'] :=.: Expr [v 'Y' := v 'a']]

testProblem4 :: UnifProblem
testProblem4 = S.fromList [ Expr [v 'X' := v 'Y'] :=.: Expr [v 'Y' := v 'A']]

testProblem5 :: UnifProblem
testProblem5 = S.fromList [ SingleSVarExpr (SetVar 0) [v 'X' := v 'a'] :=.: Expr [v 'A' := v 'a', v 'B' := v 'D'] ]

testProblem6 :: UnifProblem
testProblem6 = S.singleton $ Expr [v 'A' := v 'B', v 'C' := v 'D'] :=.: Expr [v 'x' := v 'y', v 'z' := v 'w']

testProblem7 :: UnifProblem
testProblem7 = S.singleton $ SingleSVarExpr (SetVar 0) [v 'X' := v 'a']
                            :=.: SingleSVarExpr (SetVar 0) [v 'c' := v 'd', v 'x' := v 'A']
