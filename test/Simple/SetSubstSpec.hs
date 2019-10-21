module Simple.SetSubstSpec (spec) where

import Test.Hspec
import Simple.SetSubst

import Simple.Expression
    ( SetVar(SetVar)
    , Expr(Expr, SingleSVarExpr)
    , Bind((:=))
    , Var
    , var
    , meta
    , expr
    , ssve
    )

import Data.Char(isUpper)

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
    -- Substitution
    describe "substitution" $ do
        it "{M0 → [Y=b]} (M0:[X=a]) === [Y=b,X=a]" $ do
            (SetVar 0 → (expr [v 'Y' := v 'b'])) `onExpr` (setExpr [SetVar 0] [v 'X' := v 'a'])
                `shouldBe` expr [v 'Y' := v 'b', v 'X' := v 'a']
        it "{M0 → M1:[Y=b]} (M0:[X=a]) === M1:[Y=b,X=a]" $ do
            (SetVar 0 → (setExpr [SetVar 1] [v 'Y' := v 'b'])) `onExpr` (setExpr [SetVar 0] [v 'X' := v 'a'])
                `shouldBe` setExpr [SetVar 1] [v 'Y' := v 'b', v 'X' := v 'a']
        it "{M -> [c=d]} (M:[c=d,X=a]) === [c=d,c=d,X=a]" $ do
            (SetVar 0 → (expr [v 'c' := v 'd'])) `onExpr` (setExpr [SetVar 0] [v 'c' := v 'd', v 'X' := v 'a'])
                `shouldBe` expr [v 'c' := v 'd', v 'c' := v 'd', v 'X' := v 'a']
    describe "compose" $ do
        it "{M1 → M2:[] |} {M0 → M1:[] |} === {M0 → M2:[], M1 → M2:[]|}" $ do
            (SetVar 1 → (setExpr [SetVar 2] [])) `compose` (SetVar 0 → (setExpr [SetVar 1] []))
                `shouldBe` (   (SetVar 1 → (setExpr [SetVar 2] [])) `compose`   (SetVar 0 → (setExpr [SetVar 2] []))    )
        it "{M1 → M2:[] |} {M0 → M1:[] |} === {M0 → M2:[], M1 → M2:[]|}" $ do
            (addApos (SetVar 1) → (setExpr [SetVar 2] [])) `compose` (SetVar 0 → (setExpr [addApos $ SetVar 1] []))
                `shouldBe` (   (addApos (SetVar 1) → (setExpr [SetVar 2] [])) `compose` (SetVar 0 → (setExpr [SetVar 2] []))    )
