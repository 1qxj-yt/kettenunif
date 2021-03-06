module Simple.SubstitutionSpec (spec) where

import Test.Hspec
import Simple.Substitution

import Simple.Expression
    ( Expr
    , Bind((:=))
    , Var, var, meta
    , SetVar(SetVar)
    , Token(..)
    , isMeta
    , expr
    )

import Data.Char(isUpper)
import qualified Data.Map as M

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

    -- Build
    describe "build" $ do
        it "does not allow ill-defined substitution [X→a,X→b] to be created" $ do
            (print $ build [v 'X' → v 'a', v 'X' → v 'b']) `shouldThrow` anyErrorCall

    -- Compose
    describe "compose" $ do
        it "{B→C}.{A→B} === {A→C,B→C}" $ do
            compose (v 'B' → v 'C') (v 'A' → v 'B') `shouldBe` build [v 'A' → v 'C', v 'B' → v 'C']
        it "{Y→a}.{X→Y} === {X→a,Y→a}" $ do
            compose (v 'Y' → v 'a') (v 'X' → v 'a') `shouldBe` build [v 'X' → v 'a', v 'Y' → v 'a']
        it "{X→a}.{Y→b} === {X→a,Y→b}" $ do
            compose (v 'X' → v 'a') (v 'Y' → v 'b') `shouldBe` build [v 'X' → v 'a', v 'Y' → v 'b']
        it "{X→a}.{X→b} === {X→b}" $ do
            compose (v 'X' → v 'a') (v 'X' → v 'b') `shouldBe` build [v 'X' → v 'b']
        it "{X→a}.{X→a} === {X→a}" $ do
            compose (v 'X' → v 'a') (v 'X' → v 'a') `shouldBe` build [v 'X' → v 'a']
        it "{X→Y,Y→X}^2 === id" $ do
            let s = build [v 'X' → v 'Y', v 'Y' → v 'X']
            compose s s `shouldBe` identity
        it "{M→[X=y] | X→a}.id === {M→[X=y] | X→a}" $ do
            let s = build [SetVar 0 →→ expr [v 'X' := v 'a'], v 'X' → v 'a']
            compose s identity `shouldBe` s
        it "{A->B}.{M1->[Y=b]|Y->A} === {M1->[Y=b]|A->B,Y->B}" $ do
            let a = build [v 'A' → v 'B']
                b = build [SetVar 1 →→ expr [v 'Y' := v 'b'], v 'Y' → v 'A']
                s = build [SetVar 1 →→ expr [v 'Y' := v 'b'], v 'A' → v 'B', v 'Y' → v 'B']
            compose a b `shouldBe` s

    -- Equivalence
    describe "equivalent" $ do
        it "{A→X,D→a} =~= {A→X,D→a}" $ do
            equivalent  (build [v 'A'→v 'X',v 'D'→v 'a'])
                        (build [v 'A'→v 'X',v 'D'→v 'a'])
                `shouldBe` True
        it "{A→X,D→a} =~= {A→Y,D→a}" $ do
            equivalent  (build [v 'A'→v 'X',v 'D'→v 'a'])
                        (build [v 'A'→v 'Y',v 'D'→v 'a'])
                `shouldBe` True
        it "{A→X,D→a} =~/= {A→Y,D→b}" $ do
            equivalent  (build [v 'A'→v 'X',v 'D'→v 'a'])
                        (build [v 'A'→v 'Y',v 'D'→v 'b'])
                `shouldBe` False
        it "{A→X,B→X,D→a} =~= {A→Y,B→Y,D→a}" $ do
            equivalent  (build [v 'A'→v 'X',v 'B'→v 'X',v 'D'→v 'a'])
                        (build [v 'A'→v 'Y',v 'B'→v 'Y',v 'D'→v 'a'])
                `shouldBe` True
        it "{A→X,B→X,D→a} =~/= {A→X,B→Y,D→a}" $ do
            equivalent  (build [v 'A'→v 'X',v 'B'→v 'X',v 'D'→v 'a'])
                        (build [v 'A'→v 'X',v 'B'→v 'Y',v 'D'→v 'a'])
                `shouldBe` False
        it "{A→X,B→X,D→a} =~/= {A→Y,B→Y,D→b}" $ do
            equivalent  (build [v 'A'→v 'X',v 'B'→v 'X',v 'D'→v 'a'])
                        (build [v 'A'→v 'X',v 'B'→v 'Y',v 'D'→v 'a'])
                `shouldBe` False
        it "{A→X,B→X,C→Y,D→a} =~= {A→Y,B→Y,C→Z,D→a}" $ do
            equivalent  (build [v 'A'→v 'X',v 'B'→v 'X',v 'C'→v 'Y',v 'D'→v 'a'])
                        (build [v 'A'→v 'X',v 'B'→v 'X',v 'C'→v 'Y',v 'D'→v 'a'])
                `shouldBe` True
        it "{A→X,B→X,C→Y,D→a} =~/= {A→X,B→Y,C→X,D→a}" $ do
            equivalent  (build [v 'A'→v 'X',v 'B'→v 'X',v 'C'→v 'Y',v 'D'→v 'a'])
                        (build [v 'A'→v 'X',v 'B'→v 'Y',v 'C'→v 'X',v 'D'→v 'a'])
                `shouldBe` False
        it "{A→X,B→Y} =~= {B→X,A→Y}" $ do
            equivalent  (build [v 'A'→v 'X',v 'B'→v 'Y'])
                        (build [v 'B'→v 'X',v 'A'→v 'Y'])
                `shouldBe` True
        it "{A→X,Z→Y} =~= {A→X,Z→Y}" $ do
            equivalent  (build [v 'A'→v 'X',v 'B'→v 'Y'])
                        (build [v 'B'→v 'X',v 'A'→v 'Y'])
                `shouldBe` True

    -- Test data check
    describe "isValid" $ do
        it "recognizes test case substitutions to be valid" $ do
            all isValid [transposeXY, transposeABC, transposeCBA] `shouldBe` True

    -- Substitution
    describe "substitution" $ do
        it "[X→a]{X} === {a}" $ do
            build [v 'X' → v 'a'] `onAny` V (v 'X') `shouldBe` V (v 'a')
        it "[X→a]{x} === {x}" $ do
            build [v 'X' → v 'a'] `onAny` V (v 'x') `shouldBe` V (v 'x')
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
transposeXY = build [v 'Y' → v 'X', v 'X' → v 'Y']

bindXtoY :: Token
bindXtoY = B (v 'X' := v 'Y')


transposeABC :: Substitution
transposeABC = build [v 'A' → v 'B', v 'B' → v 'C', v 'C' → v 'A']

transposeCBA :: Substitution
transposeCBA = build [v 'A' → v 'B', v 'C' → v 'A', v 'B' → v 'C']

exprAB_BB_CB :: Token
exprAB_BB_CB = E (expr [v 'A' := v 'B', v 'B' := v 'B', v 'C' := v 'B'])
