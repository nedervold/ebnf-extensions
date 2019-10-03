module Language.Ebnf.Extensions.Generators
  ( genOpt
  , genRep0
  , genRep1
  , genRepsep0
  , genRepsep1
  ) where

import Control.Monad (replicateM)
import Hedgehog
import Hedgehog.Gen
import Language.Ebnf.Extensions.Syntax
import Prelude hiding (maybe)

genOpt
  :: MonadGen m
  => m a -> m (Opt a)
genOpt = maybe

genRep0
  :: MonadGen m
  => Range Int -> m a -> m (Rep0 a)
genRep0 = list

genRep1
  :: MonadGen m
  => Range Int -> m a -> m (Rep1 a)
genRep1 = nonEmpty

genRepsep0
  :: MonadGen m
  => Range Int -> m s -> m b -> m (Repsep0 s b)
genRepsep0 rng gs gb = do
  bs <- list rng gb
  if null bs
    then pure Repsep0Nothing
    else do
      let len = length bs
      ss <- replicateM (len - 1) gs
      pure $ Repsep0Just $ mkRepsep1 bs ss

genRepsep1
  :: MonadGen m
  => Range Int -> m s -> m b -> m (Repsep1 s b)
genRepsep1 rng gs gb = do
  bs <- list rng gb
  let len = length bs
  ss <- replicateM (len - 1) gs
  pure $ mkRepsep1 bs ss

mkRepsep1 :: [b] -> [s] -> Repsep1 s b
mkRepsep1 [b] [] = Repsep1Singleton b
mkRepsep1 (b:bs) (s:ss) = Repsep1Cons b s (mkRepsep1 bs ss)
mkRepsep1 bs ss = error "mkRepsep1"
