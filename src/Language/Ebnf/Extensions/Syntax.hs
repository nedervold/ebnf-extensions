{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Ebnf.Extensions.Syntax where

import Control.Lens.TH (makeClassy)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.List.NonEmpty
import Data.Ord (comparing)
import GHC.Generics (Generic)

type Opt = Maybe

------------------------------------------------------------
type Rep0 = []

------------------------------------------------------------
type Rep1 = Data.List.NonEmpty.NonEmpty

------------------------------------------------------------
data Repsep0 s b
  = Repsep0Nothing
  | Repsep0Just { _repsep0Contents :: Repsep1 s b}
  deriving (Eq, Functor, Generic, Ord, Show)

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
data Repsep1 s b
  = Repsep1Singleton { _repsep1Body :: b}
  | Repsep1Cons { _repsep1Body :: b
               ,  _repsep1Separator :: s
               ,  _repsep1Tail :: Repsep1 s b}
  deriving (Eq, Functor, Generic, Show)

instance (Ord b, Ord s) =>
         Ord (Repsep1 s b) where
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
makeClassy ''Repsep0

makeClassy ''Repsep1
