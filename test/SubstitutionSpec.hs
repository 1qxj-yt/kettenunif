module SubstitutionSpec (spec) where

import Test.Hspec
import Substitution

import Expression
    ( Expr
    , Bind((:=))
    , Var, var, meta
    , isMeta
    )


spec :: Spec
spec = do
    describe "back-and-forth substitution" $ do
        it "equals identity" $ do
            subst `onAny` sbind `shouldBe` sbind


subst :: Substitution
subst = [meta 'Y' 0 → meta 'X' 0, meta 'X' 0 → meta 'Y' 0]

sbind :: Token
sbind = B (meta 'X' 0 := meta 'Y' 0)