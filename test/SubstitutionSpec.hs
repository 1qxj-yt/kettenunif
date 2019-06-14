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
    describe "double-application of transposition {Y→X,X→Y}" $ do
        it "equals identity on binding X:=Y" $ do
            (subst `onAny`) (subst `onAny` sbind) `shouldBe` sbind


subst :: Substitution
subst = [v 'Y' → v 'X', v 'X' → v 'Y']

sbind :: Token
sbind = B (v 'X' := v 'Y')