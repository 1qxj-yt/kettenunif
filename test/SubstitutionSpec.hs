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
    describe "single substitution X→a" $ do
        it "succeeds on single var X" $ do
            [v 'X' → v 'a'] `onAny` V (v 'X') `shouldBe` V (v 'a')
    describe "double-application of transposition {Y→X,X→Y}" $ do
        it "equals identity on binding X:=Y" $ do
            (subst `onAny`) (subst `onAny` sbind) `shouldBe` sbind

------------------------------------------------
-- Test Data
------------------------------------------------


subst :: Substitution
subst = [v 'Y' → v 'X', v 'X' → v 'Y']

sbind :: Token
sbind = B (v 'X' := v 'Y')