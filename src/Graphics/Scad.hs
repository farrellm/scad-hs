{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Graphics.Scad
  ( Facet
  , Dimension(..)
  , V2(..)
  , V3(..)
  , Model
  , SomeModel
  , Module
  , Shape
  , Form
  , Shape'
  , Form'
  , SetLike(..)
  , Union(..)
  , Intersection(..)
  , HasChildren
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
  , linearExtrude
  , projection
  , offsetR
  , offsetDelta
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
  , list
  , fa
  , fs
  , fn
  , smodule
  , children
  , render
  , defaultFacet
  , someModel
  ) where

import Graphics.Scad.Types
import Graphics.Scad.Class

import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Data.Text (pack)
import Data.Text.Prettyprint.Doc (Pretty(..), Doc, vcat)
import Polysemy
import Polysemy.Reader
import Polysemy.State


type Shape = Model 'Two
type Form = Model 'Three

type Model' d = Sem '[Reader Facet, State (Map SomeModel Text)] (Model d)
type Shape' = Model' 'Two
type Form' = Model' 'Three


tau :: Floating a => a
tau = 2 * pi

defaultFacet :: Facet
defaultFacet = Facet {_fa = Nothing, _fs = Nothing, _fn = Nothing}

render :: (Pretty (Model d)) => Model' d -> Doc ann
render mdl =
  let (ts, m) = run . runState mempty $ runReader defaultFacet mdl
      ts' = fmap mkMod $ M.toList ts
   in vcat (fmap pretty ts' <> [pretty m])
  where
    mkMod (Model2 m, n) = Module2 n m
    mkMod (Model3 m, n) = Module3 n m


circle :: (Member (Reader Facet) r) => Double -> Sem r Shape
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


sphere :: (Member (Reader Facet) r) => Double -> Sem r Form
sphere r = Sphere r <$> ask

cube :: (Applicative m) => Double -> m Form
cube r = pure (Cube r True)

cube' :: (Applicative m) => Double -> m Form
cube' r = pure (Cube r False)

box :: (Applicative m) => V3 Double -> m Form
box r = pure (Box r True)

box' :: (Applicative m) => V3 Double -> m Form
box' r = pure (Box r False)

cylinder :: (Member (Reader Facet) r) => Double -> Double -> Sem r Form
cylinder h r = Cylinder h r True <$> ask

cylinder' :: (Member (Reader Facet) r) => Double -> Double -> Sem r Form
cylinder' h r = Cylinder h r False <$> ask

cylinder2 :: (Member (Reader Facet) r) => Double -> (Double, Double) -> Sem r Form
cylinder2 h (r1, r2) = Cylinder2 h r1 r2 True <$> ask

cylinder2' :: (Member (Reader Facet) r) => Double -> (Double, Double) -> Sem r Form
cylinder2' h (r1, r2) = Cylinder2 h r1 r2 False <$> ask

linearExtrude :: (Member (Reader Facet) r) => Double -> Bool -> Int -> Double -> Sem r Shape -> Sem r Form
linearExtrude h c v t m = LinearExtrude h c v (Radian t) <$> ask <*> m


projection :: (Functor m) => Bool -> m Form -> m Shape
projection c m = Projection c <$> m

offsetR :: (Functor m) => Double -> Bool -> m Shape -> m Shape
offsetR r c m = Offset (OffsetR r) c <$> m

offsetDelta :: (Functor m) => Double -> Bool -> m Shape -> m Shape
offsetDelta d c m = Offset (OffsetDelta d) c <$> m

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

list :: (Applicative m) => [m (Model d)] -> m (Model d)
list ms = ModelList <$> sequenceA ms


fa :: (Member (Reader Facet) r) => Double -> Sem r (Model d) -> Sem r (Model d)
fa x mdl = local (\f -> f {_fa = Just x}) mdl

fs :: (Member (Reader Facet) r) => Double -> Sem r (Model d) -> Sem r (Model d)
fs x mdl = local (\f -> f {_fs = Just x}) mdl

fn :: (Member (Reader Facet) r) => Double -> Sem r (Model d) -> Sem r (Model d)
fn x mdl = local (\f -> f {_fn = Just x}) mdl


data HasChildren (d :: Dimension) m a where
  Children :: HasChildren d m (Model d)

makeSem ''HasChildren

runChildren ::
     Sem r (Model e) -> Sem (HasChildren e ': r) (Model d) -> Sem r (Model d)
runChildren _ = do
  interpret $ \case Graphics.Scad.Children -> pure Graphics.Scad.Types.Children


class Apply (d :: Dimension) where
  apply :: Text -> Model d -> Model e

instance Apply 'Two where
  apply = Apply2

instance Apply 'Three where
  apply = Apply3

smodule ::
     (IsSomeModel (Model e), Apply d, Member (State (Map SomeModel Text)) r)
  => [Sem (HasChildren d ': r) (Model e)]
  -> Sem r (Model d)
  -> Sem r (Model e)
smodule b c = do
  b' <- someModel <$> runChildren c (list b)
  mName <- M.lookup b' <$> get
  name <-
    case mName of
      Nothing -> do
        z <- M.size <$> get
        let n = "mdl_" <> pack (show z)
        modify (M.insert b' n)
        pure n
      Just n -> pure n
  apply name <$> c
