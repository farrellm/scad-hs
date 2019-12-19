{-# LANGUAGE FlexibleInstances #-}

module Graphics.Scad.Class
  ( SetLike(..)
  , Union(..)
  , Intersection(..)
  ) where

import Graphics.Scad.Types
import Control.Applicative (liftA2)

infixl 6 <+>
infixl 6 <#>
infixl 6 <->
class SetLike a where
  (<+>) :: a -> a -> a
  (<#>) :: a -> a -> a
  (<->) :: a -> a -> a

instance SetLike (Model d) where
  Union' xs        <+> Union' ys        = Union' (xs <> ys)
  x                <+> Union' ys        = Union' ([x] <> ys)
  Union' xs        <+> y               = Union' (xs <> [y])
  x                <+> y               = Union' [x, y]

  Intersection' xs <#> Intersection' ys = Intersection' (xs <> ys)
  x                <#> Intersection' ys = Intersection' ([x] <> ys)
  Intersection' xs <#> y               = Intersection' (xs <> [y])
  x                <#> y               = Intersection' [x, y]

  Difference x y   <-> a               = Difference x (y <+> a)
  x                <-> y               = Difference x y

instance (SetLike a, Applicative m) => SetLike (m a) where
  (<+>) = liftA2 (<+>)
  (<#>) = liftA2 (<#>)
  (<->) = liftA2 (<->)

newtype Union d = Union { getUnion :: d }

instance (SetLike a) => Semigroup (Union a) where
  Union a <> Union b = Union (a <+> b)

instance Monoid (Union (Model d)) where
  mempty = Union (Union' [])

instance (Applicative m) => Monoid (Union (m (Model d))) where
  mempty = Union (pure $ Union' [])

newtype Intersection a = Intersection { getIntersection :: a }

instance (SetLike a) => Semigroup (Intersection a) where
  Intersection a <> Intersection b = Intersection (a <#> b)

instance Monoid (Intersection (Model d)) where
  mempty = Intersection (Intersection' [])

instance (Applicative m) => Monoid (Intersection (m (Model d))) where
  mempty = Intersection (pure (Intersection' []))
