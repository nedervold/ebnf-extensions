-- | Parsers for extensions to BNF.
module Language.Ebnf.Extensions.Parsers
  ( parseOpt
  , parseRep0
  , parseRep1
  , parseRepsep0
  , parseRepsep1
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus(mplus))
import Control.Monad.Combinators (many, optional)
import Control.Monad.Combinators.NonEmpty (some)
import Language.Ebnf.Extensions.Syntax

-- | Given a parser for an element, a parser for an optional
-- element.  A synonym for 'Control.Monad.Combinators.optional'.
parseOpt :: Alternative f => f b -> f (Opt b)
parseOpt = optional

-- | Given a parser for an element, a parser for a list of
-- elements.  A synonym for 'Control.Monad.Combinators.many'.
parseRep0 :: MonadPlus m => m b -> m (Rep0 b)
parseRep0 = many

-- | Given a parser for an element, a parser for a non-empty list of
-- elements.  A synonym for 'Control.Monad.Combinators.NonEmpty.some'.
parseRep1 :: MonadPlus m => m b -> m (Rep1 b)
parseRep1 = some

-- | Given a parser for an element and a parser for the separator, a
-- parser for a possibly empty list of separated elements.
parseRepsep0 :: MonadPlus m => m s -> m b -> m (Repsep0 s b)
-- TODO Could/should this be lowered to Alternative?
parseRepsep0 ps pb =
  (Repsep0Just <$> parseRepsep1 ps pb) `mplus` pure Repsep0Nothing

-- | Given a parser for an element and a parser for the separator, a
-- parser for a non-empty list of separated elements.
parseRepsep1 :: MonadPlus m => m s -> m b -> m (Repsep1 s b)
-- TODO Could/should this be lowered to Alternative?
parseRepsep1 ps pb = do
  b <- pb
  (do s <- ps
      rs1 <- parseRepsep1 ps pb
      pure $ Repsep1Cons b s rs1) `mplus`
    pure (Repsep1Singleton b)
