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
    describe "substitution" $ do
        it "[X→a]{X} === {a}" $ do
            [v 'X' → v 'a'] `onAny` V (v 'X') `shouldBe` V (v 'a')
        it "[X→a]{x} === {x}" $ do
            [v 'X' → v 'a'] `onAny` V (v 'x') `shouldBe` V (v 'x')
        it "[Y→X,X→Y]{X:=Y} === {Y:=X}" $ do
            transposeXY `onAny` bindXtoY `shouldBe` B (v 'Y' := v 'X')
        it "([Y→X,X→Y]^2){X:=Y} === {X:=Y}" $ do
            (2 `times` (transposeXY `onAny`)) bindXtoY `shouldBe` bindXtoY
        it "([C→A,A→B,B→C]^3){A:=B,B:=B,C:=B} === {A:=B,B:=B,C:=B}" $ do
            (3 `times` (transposeABC `onAny`)) exprAB_BB_CB `shouldBe` exprAB_BB_CB
        it "([A→B,C→A,B→A]^3){A:=B,B:=B,C:=B} === {A:=B,B:=B,C:=B}" $ do
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


