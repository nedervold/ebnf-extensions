-- | Generators for extensions to BNF.
module Language.Ebnf.Extensions.Generators
  ( genOpt
  , genRep0
  , genRep1
  , genRepsep0
  , genRepsep1
  ) where

import Control.Monad (replicateM)
import Hedgehog (MonadGen, Range)
import Hedgehog.Gen (list, maybe, nonEmpty)
import Language.Ebnf.Extensions.Syntax
  ( Opt
  , Rep0
  , Rep1
  , Repsep0(..)
  , Repsep1(..)
  )
import Prelude hiding (maybe)

-- | Given the generator for an element, the generator for an optional
-- element.  A synonym for 'Hedgehog.Gen.maybe'.
genOpt :: MonadGen m => m a -> m (Opt a)
genOpt = maybe

-- | Given the generator for an element, the generator for a list of
-- elements.  A synonym for 'Hedgehog.Gen.list'.
genRep0 :: MonadGen m => Range Int -> m a -> m (Rep0 a)
genRep0 = list

-- | Given the generator for an element, the generator for a non-empty
-- list of elements.  A synonym for 'Hedgehog.Gen.nonEmpty'.
genRep1 :: MonadGen m => Range Int -> m a -> m (Rep1 a)
genRep1 = nonEmpty

-- | Given the generator for a separator and for an element, the
-- generator for a possibly empty list of elements separated by the
-- separator.
genRepsep0 :: MonadGen m => Range Int -> m s -> m b -> m (Repsep0 s b)
genRepsep0 rng gs gb = do
  bs <- list rng gb
  if null bs
    then pure Repsep0Nothing
    else do
      let len = length bs
      ss <- replicateM (len - 1) gs
      pure $ Repsep0Just $ mkRepsep1 bs ss

-- | Given the generator for a separator and for an element, the
-- generator for a non-empty list of elements separated by the
-- separator.
genRepsep1 :: MonadGen m => Range Int -> m s -> m b -> m (Repsep1 s b)
genRepsep1 rng gs gb = do
  bs <- list rng gb
  let len = length bs
  ss <- replicateM (len - 1) gs
  pure $ mkRepsep1 bs ss

-- | Weave a list of elements and separators into a 'Repsep1'.
mkRepsep1 :: [b] -> [s] -> Repsep1 s b
mkRepsep1 [b] [] = Repsep1Singleton b
mkRepsep1 (b:bs) (s:ss) = Repsep1Cons b s (mkRepsep1 bs ss)
-- should be impossible by construction
mkRepsep1 _bs _ss = error "mkRepsep1: bodies and separators mismatched"
