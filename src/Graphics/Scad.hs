{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Scad
  ( Facet
  , Dimension(..)
  , V2(..)
  , V3(..)
  , Model
  , Shape
  , Form
  , Shape'
  , Form'
  , SetLike(..)
  , Union(..)
  , Intersection(..)
  , tau
  , circle
  , square
  , square'
  , rectangle
  , rectangle'
  , convex
  , polygon
  , sphere
  , cube
  , cube'
  , box
  , box'
  , cylinder
  , cylinder'
  , cylinder2
  , cylinder2'
  , projection
  , project
  , cut
  , translate
  , rotate
  , rotate'
  , rotate2d
  , scale
  , resize
  , mirror
  , hull
  , minkowski
  , union
  , intersection
  , difference
  , fa
  , fs
  , fn
  , render
  , defaultFacet
  ) where

import Graphics.Scad.Types
import Graphics.Scad.Class

import Control.Applicative (liftA2)
import Control.Monad.Reader
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Data.Text.Prettyprint.Doc


type Shape = Model 'Two
type Form = Model 'Three

type Shape' = Reader Facet (Model 'Two)
type Form' = Reader Facet (Model 'Three)


tau :: Floating a => a
tau = 2 * pi

defaultFacet :: Facet
defaultFacet = Facet {_fa = Nothing, _fs = Nothing, _fn = Nothing}

render :: (Pretty (Model d)) => Reader Facet (Model d) -> Doc ann
render mdl = pretty $ runReader mdl defaultFacet


circle :: (MonadReader Facet m) => Double -> m Shape
circle r = Circle r <$> ask

square :: (Applicative m) => Double -> m Shape
square r = pure (Square r True)

square' :: (Applicative m) => Double -> m Shape
square' r = pure (Square r False)

rectangle :: (Applicative m) => V2 Double -> m Shape
rectangle r = pure (Rectangle r True)

rectangle' :: (Applicative m) => V2 Double -> m Shape
rectangle' r = pure (Rectangle r False)

convex :: (Applicative m) => [V2 Double] -> m Shape
convex vs = pure (Polygon vs [] Nothing)

polygon :: (Applicative m) => [V2 Double] -> [[Int]] -> Maybe Int -> m Shape
polygon vs ps c = pure (Polygon vs ps c)


sphere :: (MonadReader Facet m) => Double -> m Form
sphere r = Sphere r <$> ask

cube :: (Applicative m) => Double -> m Form
cube r = pure (Cube r True)

cube' :: (Applicative m) => Double -> m Form
cube' r = pure (Cube r False)

box :: (Applicative m) => V3 Double -> m Form
box r = pure (Box r True)

box' :: (Applicative m) => V3 Double -> m Form
box' r = pure (Box r False)

cylinder :: (MonadReader Facet m) => Double -> Double -> m Form
cylinder h r = Cylinder h r True <$> ask

cylinder' :: (MonadReader Facet m) => Double -> Double -> m Form
cylinder' h r = Cylinder h r False <$> ask

cylinder2 :: (MonadReader Facet m) => Double -> (Double, Double) -> m Form
cylinder2 h (r1, r2) = Cylinder2 h r1 r2 True <$> ask

cylinder2' :: (MonadReader Facet m) => Double -> (Double, Double) -> m Form
cylinder2' h (r1, r2) = Cylinder2 h r1 r2 False <$> ask


projection :: (Functor m) => Bool -> m Form -> m Shape
projection c m = Projection c <$> m

project :: (Functor m) => m Form -> m Shape
project m = Projection False <$> m

cut :: (Functor m) => m Form -> m Shape
cut m = Projection True <$> m


translate :: (Functor m) => V d Double -> m (Model d) -> m (Model d)
translate v mdl = Translate v <$> mdl

rotate ::
     (Functor m)
  => Double
  -> V 'Three Double
  -> m (Model 'Three)
  -> m (Model 'Three)
rotate a v mdl = RotateV (Radian a) v <$> mdl

rotate' ::
     (Functor m) => V 'Three Double -> m (Model 'Three) -> m (Model 'Three)
rotate' a mdl = RotateA (Radian <$> a) <$> mdl

rotate2d :: (Functor m) => Double -> m (Model 'Two) -> m (Model 'Two)
rotate2d a mdl = RotateA (Radian <$> V3 0 0 a) <$> mdl

scale :: (Functor m) => V d Double -> m (Model d) -> m (Model d)
scale v mdl = Scale v <$> mdl

resize :: (Functor m) => V d Double -> m (Model d) -> m (Model d)
resize v mdl = Resize v <$> mdl

mirror :: (Functor m) => V d Double -> m (Model d) -> m (Model d)
mirror v mdl = Mirror v <$> mdl

hull :: (Applicative m) => [m (Model d)] -> m (Model d)
hull ms = Hull <$> sequenceA ms

minkowski :: (Applicative m) => [m (Model d)] -> m (Model d)
minkowski ms = Minkowski <$> sequenceA ms


union :: (Applicative m) => [m (Model d)] -> m (Model d)
union ms = getUnion . mconcat $ fmap Union ms

intersection :: (Applicative m) => [m (Model d)] -> m (Model d)
intersection ms = getIntersection . mconcat $ fmap Intersection ms

difference :: (Applicative m) => m (Model d) -> [m (Model d)] -> m (Model d)
difference x ys = liftA2 Difference x (union ys)


fa :: (MonadReader Facet m) => Double -> m (Model d) -> m (Model d)
fa x mdl = local (\f -> f {_fa = Just x}) mdl

fs :: (MonadReader Facet m) => Double -> m (Model d) -> m (Model d)
fs x mdl = local (\f -> f {_fs = Just x}) mdl

fn :: (MonadReader Facet m) => Double -> m (Model d) -> m (Model d)
fn x mdl = local (\f -> f {_fn = Just x}) mdl
