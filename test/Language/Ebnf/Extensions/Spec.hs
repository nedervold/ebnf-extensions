module Language.Ebnf.Extensions.Spec
  ( spec_extensions
  ) where

import Hedgehog

-- prop_: QuickCheck properties.
-- scprop_: SmallCheck properties.
-- hprop_: Hedgehog properties.
-- unit_: HUnit test cases.
-- spec_: Hspec specifications.
-- test_: Tasty TestTrees.
import Hedgehog.Classes
import Hedgehog.Gen
import Hedgehog.Range
import Language.Ebnf.Extensions.Generators
import Language.Ebnf.Extensions.Syntax
import Test.Tasty.Hspec

spec_extensions :: Spec
spec_extensions = do
  let rng = linear 1 100
  describe "Language.Ebnf.Extensions" $ do
    it "obeys its laws" $ do
      passed <-
        lawsCheckMany
          [ ( "Repsep0"
            , [ foldableLaws $ genRepsep0 rng genComma
              , traversableLaws $ genRepsep0 rng genComma
              , bifoldableLaws $ genRepsep0 rng
              , bitraversableLaws $ genRepsep0 rng
              ])
          , ( "Repsep1"
            , [ ordLaws $ genRepsep1 rng genComma (genIdent rng)
              , foldableLaws $ genRepsep1 rng genComma
              , traversableLaws $ genRepsep1 rng genComma
              , bifoldableLaws $ genRepsep1 rng
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
