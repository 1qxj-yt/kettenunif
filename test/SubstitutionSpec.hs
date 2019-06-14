module SubstitutionSpec (spec) where

import Test.Hspec
import Substitution

import Expression
    ( Expr
    , Bind((:=))
    , Var, var, meta
    , isMeta
    )

import Data.Char(isUpper)

------------------------------------------------
-- Utility Functions
------------------------------------------------

-- Easy creation of variables.
v :: Char -> Var
v c = (if ($c) isUpper then meta else var) c 0

times :: Int -> (a -> a) -> (a -> a)
times 0 _ = id
times 1 f = f
times n f = f . ((n-1) `times` f)


------------------------------------------------
-- Specification
------------------------------------------------

spec :: Spec
spec = do
    -- Constructor
    describe "(→)" $ do
        it "does not allow invalid substitution [x→a] to be created" $ do
            (print (v 'x' → v 'a')) `shouldThrow` anyErrorCall

    -- Test data check
    describe "isWellDef" $ do
        it "recognizes substitution [X→a,X→b] to be ill-defined" $ do
            ($[v 'X' → v 'a', v 'X' → v 'b']) isWellDef `shouldBe` False
        it "recognizes test case substitutions to be well-defined" $ do
            all isWellDef [transposeXY, transposeABC, transposeCBA] `shouldBe` True

    -- Substitution
    describe "single substitution X→a" $ do
        it "succeeds on single var X" $ do
            [v 'X' → v 'a'] `onAny` V (v 'X') `shouldBe` V (v 'a')
    describe "double-application of transposition {Y→X,X→Y}" $ do
        it "equals identity on binding X:=Y" $ do
            (2 `times` (transposeXY `onAny`)) bindXtoY `shouldBe` bindXtoY
    describe "triple-application of permutation {C→A,A→B,B→C}" $ do
        it "equals identity on expression {A:=B,B:=B,C:=B}" $ do
            (3 `times` (transposeABC `onAny`)) exprAB_BB_CB `shouldBe` exprAB_BB_CB
    describe "triple-application of reordered permutation {A→B,C→A,B→A}" $ do
        it "equals identity on expression {A:=B,B:=B,C:=B}" $ do
            (3 `times` (transposeCBA `onAny`)) exprAB_BB_CB `shouldBe` exprAB_BB_CB


------------------------------------------------
-- Test Data
------------------------------------------------

transposeXY :: Substitution
transposeXY = [v 'Y' → v 'X', v 'X' → v 'Y']

bindXtoY :: Token
bindXtoY = B (v 'X' := v 'Y')


transposeABC :: Substitution
transposeABC = [v 'A' → v 'B', v 'B' → v 'C', v 'C' → v 'A']

transposeCBA :: Substitution
transposeCBA = [v 'A' → v 'B', v 'C' → v 'A', v 'B' → v 'C']

exprAB_BB_CB :: Token
exprAB_BB_CB = E [v 'A' := v 'B', v 'B' := v 'B', v 'C' := v 'B']


