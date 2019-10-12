module Simple.UnifProblemSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Simple.UnifProblem

import Simple.SoundnessAuto.Bi_Mset
import Simple.Expression

import qualified Data.Set as S

spec :: Spec
spec =
    describe "Ordering for termination" $ do
        it "M0:[] =. e is always smaller than M:[..] =. e2 (QuickCheck IO)" $
            quickCheck minIsSmaller
        prop "same as above (with hspec internal QuickCheck)" minIsSmaller
        prop "smaller is shorter" shorterIsSmaller

minimalSSV = SingleSVarExpr (SetVar 0) mempty

minIsSmaller :: UnifProblemEl -> Expr -> Property
minIsSmaller p e =
    leftIsSSV p ==>
        E minimalSSV :=?: E e <= S.findMin (equations (probToSolver $ S.singleton p))

shorterIsSmaller :: UnifProblemEl -> UnifProblemEl -> Property
shorterIsSmaller p1 p2 =
    leftIsSSV p1 && leftIsSSV p2 && length (bindsOf $ left p1) < length (bindsOf $ left p2) ==>
        p1 <= p2

leftIsSSV :: UnifProblemEl -> Bool
leftIsSSV (SingleSVarExpr _ _ :=.: _)   = True
leftIsSSV _                             = False

left :: UnifProblemEl -> Expr
left (e :=.: _) = e


bindsOf :: Expr -> Binds
bindsOf (SingleSVarExpr _ b) = b
bindsOf (Expr b) = b
