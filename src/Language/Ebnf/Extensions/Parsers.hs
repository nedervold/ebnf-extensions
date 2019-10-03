module Language.Ebnf.Extensions.Parsers where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus(mplus))
import Control.Monad.Combinators (optional, many)
import Control.Monad.Combinators.NonEmpty (some)
import Language.Ebnf.Extensions.Syntax

parseOpt
  :: Alternative f
  => f b -> f (Opt b)
parseOpt = optional

parseRep0
  :: MonadPlus m
  => m b -> m (Rep0 b)
parseRep0 = many

parseRep1
  :: MonadPlus m
  => m b -> m (Rep1 b)
parseRep1 = some

parseRepsep0
  :: MonadPlus m
  => m s -> m b -> m (Repsep0 s b)
-- Could/should this be lowered to Alternative?
parseRepsep0 ps pb =
  (Repsep0Just <$> parseRepsep1 ps pb) `mplus` pure Repsep0Nothing

parseRepsep1
  :: MonadPlus m
  => m s -> m b -> m (Repsep1 s b)
-- Could/should this be lowered to Alternative?
parseRepsep1 ps pb = do
  b <- pb
  (do s <- ps
      rs1 <- parseRepsep1 ps pb
      pure $ Repsep1Cons b s rs1) `mplus`
    pure (Repsep1Singleton b)
