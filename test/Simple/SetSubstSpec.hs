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
            (SetVar 0 → (Expr [v 'Y' := v 'b'])) `onExpr` (SingleSVarExpr (SetVar 0) [v 'X' := v 'a'])
                `shouldBe` Expr [v 'Y' := v 'b', v 'X' := v 'a']
        it "{M0 → M1:[Y=b]} (M0:[X=a]) === M1:[Y=b,X=a]" $ do
            (SetVar 0 → (SingleSVarExpr (SetVar 1) [v 'Y' := v 'b'])) `onExpr` (SingleSVarExpr (SetVar 0) [v 'X' := v 'a'])
                `shouldBe` SingleSVarExpr (SetVar 1) [v 'Y' := v 'b', v 'X' := v 'a']
        it "{M -> [c=d]} (M:[c=d,X=a]) === [c=d,X=a]" $ do
            (SetVar 0 → (Expr [v 'c' := v 'd'])) `onExpr` (SingleSVarExpr (SetVar 0) [v 'c' := v 'd', v 'X' := v 'a'])
                `shouldBe` Expr [v 'c' := v 'd', v 'X' := v 'a']
