-- | Datatypes for extensions to BNF.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Ebnf.Extensions.Syntax
    -- * Opt
  ( Opt
    -- * Rep0
  , Rep0
    -- * Rep1
  , Rep1
  -- * Repsep0
  , Repsep0(..)
  , repsep0Contents
  -- * Repsep1
  , Repsep1(..)
  , repsep1Body
  , repsep1Separator
  , repsep1Tail
  ) where

import Data.Bifoldable (Bifoldable(bifoldMap))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.Data (Data, Typeable)
import qualified Data.List.NonEmpty
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses)

-- | A synonym for 'Maybe'.
type Opt = Maybe

------------------------------------------------------------
-- | A synonym for lists.
type Rep0 = []

------------------------------------------------------------
-- | A synonym for non-empty lists.
type Rep1 = Data.List.NonEmpty.NonEmpty

------------------------------------------------------------
-- | A possibly empty list of elements with separators.
data Repsep0 s b
  = Repsep0Nothing -- ^ an empty list
  | Repsep0Just
      { _repsep0Contents :: Repsep1 s b
      } -- ^ a non-empty  list
  deriving (Eq, Data, Functor, Generic, Ord, Show, Typeable)

instance Foldable (Repsep0 s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (Repsep0 s) where
  traverse = bitraverse pure

instance Bifunctor Repsep0 where
  bimap _f _g Repsep0Nothing = Repsep0Nothing
  bimap f g (Repsep0Just rs1) = Repsep0Just $ bimap f g rs1

instance Bifoldable Repsep0 where
  bifoldMap _f _g Repsep0Nothing = mempty
  bifoldMap f g (Repsep0Just rs1) = bifoldMap f g rs1

instance Bitraversable Repsep0 where
  bitraverse _f _g Repsep0Nothing = pure Repsep0Nothing
  bitraverse f g (Repsep0Just rs1) = Repsep0Just <$> bitraverse f g rs1

------------------------------------------------------------
-- | A non-empty  list of elements with separators.
data Repsep1 s b
  = Repsep1Singleton
      { _repsep1Body :: b -- ^ the first element
      } -- ^ a singleton list
  | Repsep1Cons
      { _repsep1Body :: b -- ^ the first element
      , _repsep1Separator :: s -- ^ the first separator
      , _repsep1Tail :: Repsep1 s b -- ^ the rest of the list
      }
  deriving (Eq, Data, Functor, Generic, Show, Typeable)

instance (Ord b, Ord s) => Ord (Repsep1 s b) where
  compare = comparing f
    where
      f :: Repsep1 s b -> (b, Maybe (s, Repsep1 s b))
      f (Repsep1Singleton b) = (b, Nothing)
      f (Repsep1Cons b s rs1) = (b, Just (s, rs1))

-- The automatic derivation wouldn't be quite right.
instance Foldable (Repsep1 s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (Repsep1 s) where
  traverse = bitraverse pure

instance Bifunctor Repsep1 where
  bimap _f g (Repsep1Singleton b) = Repsep1Singleton $ g b
  bimap f g (Repsep1Cons b s rs1) = Repsep1Cons (g b) (f s) (bimap f g rs1)

instance Bifoldable Repsep1 where
  bifoldMap _f g (Repsep1Singleton b) = g b
  bifoldMap f g (Repsep1Cons b s rs1) =
    mconcat [g b, f s, bifoldMap f g rs1]

instance Bitraversable Repsep1 where
  bitraverse _f g (Repsep1Singleton b) = Repsep1Singleton <$> g b
  bitraverse f g (Repsep1Cons b s rs1) =
    Repsep1Cons <$> g b <*> f s <*> bitraverse f g rs1

------------------------------------------------------------
makeLenses ''Repsep0

makeLenses ''Repsep1
