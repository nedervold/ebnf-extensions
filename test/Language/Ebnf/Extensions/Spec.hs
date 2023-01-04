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
import Data.Void (Void)
import Hedgehog (Gen, Range)
import Hedgehog.Classes
  ( bifoldableFunctorLaws
  , bifoldableLaws
  , bifunctorLaws
  , bitraversableLaws
  , eqLaws
  , foldableLaws
  , functorLaws
  , lawsCheckMany
  , ordLaws
  , traversableLaws
  )
import Hedgehog.Gen (alpha, alphaNum, list)
import Hedgehog.Range (linear)
import Language.Ebnf.Extensions.Generators (genRepsep0, genRepsep1)
import Language.Ebnf.Extensions.Parsers (parseRepsep0, parseRepsep1)
import Language.Ebnf.Extensions.Syntax (Repsep0(..), Repsep1(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Char (char)

spec_extensions :: Spec
spec_extensions = do
  let rng = linear 1 100
  describe "Language.Ebnf.Extensions" $ do
    it "obeys typeclass laws" $ do
      passed <-
        lawsCheckMany
          [ ( "Repsep0"
            , [ eqLaws $ genRepsep0 rng genComma (genIdent rng)
              , ordLaws $ genRepsep0 rng genComma (genIdent rng)
              , foldableLaws $ genRepsep0 rng genComma
              , functorLaws $ genRepsep0 rng genComma
              , traversableLaws $ genRepsep0 rng genComma
              , bifoldableLaws $ genRepsep0 rng
              , bifoldableFunctorLaws $ genRepsep0 rng
              , bifunctorLaws $ genRepsep0 rng
              , bitraversableLaws $ genRepsep0 rng
              ])
          , ( "Repsep1"
            , [ eqLaws $ genRepsep1 rng genComma (genIdent rng)
              , ordLaws $ genRepsep1 rng genComma (genIdent rng)
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
    describe "parsing Repsep0" $ do
      it "parses nothing" $
        parse parseRs0 "<test>" "" `shouldParse` Repsep0Nothing
      it "parses \"x\"" $
        parse parseRs0 "<test>" "x" `shouldParse`
        Repsep0Just (Repsep1Singleton 'x')
      it "parses \"x,x\"" $
        parse parseRs0 "<test>" "x,x" `shouldParse`
        Repsep0Just (Repsep1Cons 'x' ',' $ Repsep1Singleton 'x')
      it "doesn't parse \"x,y\"" $
        parse parseRs0 "<test>" `shouldFailOn` "x,y"
      it "doesn't parse \"x,x,\"" $
        parse parseRs0 "<test>" `shouldFailOn` "x,x,"
      it "parses \"x,x,x\"" $
        parse parseRs0 "<test>" "x,x,x" `shouldParse`
        Repsep0Just
          (Repsep1Cons 'x' ',' $ Repsep1Cons 'x' ',' $ Repsep1Singleton 'x')
    describe "parsing Repsep1" $ do
      it "doesn't parse nothing" $ parse parseRs1 "<test>" `shouldFailOn` ""
      it "parses \"x\"" $
        parse parseRs1 "<test>" "x" `shouldParse` Repsep1Singleton 'x'
      it "parses \"x,x\"" $
        parse parseRs1 "<test>" "x,x" `shouldParse`
        Repsep1Cons 'x' ',' (Repsep1Singleton 'x')
      it "doesn't parse \"x,y\"" $
        parse parseRs1 "<test>" `shouldFailOn` "x,y"
      it "doesn't parse \"x,x,\"" $
        parse parseRs1 "<test>" `shouldFailOn` "x,x,"
      it "parses \"x,x,x\"" $
        parse parseRs1 "<test>" "x,x,x" `shouldParse`
        Repsep1Cons 'x' ',' (Repsep1Cons 'x' ',' $ Repsep1Singleton 'x')

parseRs0 :: Parsec Void String (Repsep0 Char Char)
parseRs0 = parseRepsep0 (char ',') (char 'x')

parseRs1 :: Parsec Void String (Repsep1 Char Char)
parseRs1 = parseRepsep1 (char ',') (char 'x')

genIdent :: Range Int -> Gen String
genIdent rng = do
  hd <- alpha
  tl <- list (fmap (\n -> n - 1) rng) alphaNum
  pure (hd : tl)

genComma :: Gen String
genComma = pure ","
