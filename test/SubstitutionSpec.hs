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

-- Easy creation of variables.
v :: Char -> Var
v c = (if ($c) isUpper then meta else var) c 0


spec :: Spec
spec = do
    describe "back-and-forth substitution" $ do
        it "equals identity" $ do
            subst `onAny` sbind `shouldBe` sbind


subst :: Substitution
subst = [v 'Y' → v 'X', v 'X' → v 'Y']

sbind :: Token
sbind = B (v 'X' := v 'Y')