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
