module Language.Ebnf.Extensions.Spec
  ( spec_extensions
  ) where

{-
-- prop_: QuickCheck properties.
-- scprop_: SmallCheck properties.
-- hprop_: Hedgehog properties.
-- unit_: HUnit test cases.
-- spec_: Hspec specifications.
-- test_: Tasty TestTrees.
-}
import Hedgehog (Gen, Range)
import Hedgehog.Classes
       (foldableLaws, functorLaws, traversableLaws, bifoldableLaws,
        bifoldableFunctorLaws, bifunctorLaws, bitraversableLaws, ordLaws,
        lawsCheckMany)
import Hedgehog.Gen (alpha, alphaNum, list)
import Hedgehog.Range (linear)
import Language.Ebnf.Extensions.Generators (genRepsep0, genRepsep1)
import Language.Ebnf.Extensions.Syntax ()
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

spec_extensions :: Spec
spec_extensions = do
  let rng = linear 1 100
  describe "Language.Ebnf.Extensions" $
    it "obeys its laws" $ do
      passed <-
        lawsCheckMany
          [ ( "Repsep0"
            , [ foldableLaws $ genRepsep0 rng genComma
              , functorLaws $ genRepsep0 rng genComma
              , traversableLaws $ genRepsep0 rng genComma
              , bifoldableLaws $ genRepsep0 rng
              , bifoldableFunctorLaws $ genRepsep0 rng
              , bifunctorLaws $ genRepsep0 rng
              , bitraversableLaws $ genRepsep0 rng
              ])
          , ( "Repsep1"
            , [ ordLaws $ genRepsep1 rng genComma (genIdent rng)
              , foldableLaws $ genRepsep1 rng genComma
              , functorLaws $ genRepsep1 rng genComma
              , traversableLaws $ genRepsep1 rng genComma
              , bifoldableLaws $ genRepsep1 rng
              , bifoldableFunctorLaws $ genRepsep1 rng
              , bifunctorLaws $ genRepsep1 rng
              , bitraversableLaws $ genRepsep1 rng
              ])
          ]
      passed `shouldBe` True

genIdent :: Range Int -> Gen String
genIdent rng = do
  hd <- alpha
  tl <- list (fmap (\n -> n - 1) rng) alphaNum
  pure (hd : tl)

genComma :: Gen String
genComma = pure ","
