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

minimalSSV = setExpr [SetVar 0] []

minIsSmaller :: UnifProblemEl -> Expr -> Property
minIsSmaller p e =
    leftHasSetVars p ==>
        E minimalSSV :=?: E e <= S.findMin (probToSolver $ S.singleton p)

shorterIsSmaller :: UnifProblemEl -> UnifProblemEl -> Property
shorterIsSmaller p1 p2 =
    leftHasSetVars p1 && leftHasSetVars p2 && length (bindsOf $ left p1) < length (bindsOf $ left p2) ==>
        p1 <= p2

leftHasSetVars :: UnifProblemEl -> Bool
leftHasSetVars (e :=.: _) = let (svs,bs) = decompose e in not (null svs)

left :: UnifProblemEl -> Expr
left (e :=.: _) = e


bindsOf :: Expr -> Binds
bindsOf e = snd $ decompose e
