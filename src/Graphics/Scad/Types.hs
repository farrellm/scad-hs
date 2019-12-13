{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Scad.Types
  ( Radian(..)
  , Facet(..)
  , Dimension(..)
  , V
  , Model(..)
  ) where

import Graphics.Scad.Orphan ()

import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Data.Maybe (catMaybes)
import Data.Text.Prettyprint.Doc
import Data.Text (Text)

newtype Radian = Radian Double
  deriving (Show)

instance Pretty Radian where
  pretty (Radian r) = pretty (180 * r / pi)

data Facet =
  Facet
    { _fa :: Maybe Double
    , _fs :: Maybe Double
    , _fn :: Maybe Double
    }
  deriving (Show)

data Dimension = Two | Three
  deriving (Show)

type family V d where
  V 'Two = V2
  V 'Three = V3

data Model d where
  Zero :: Model d
  One :: Model d

  Circle :: Double -> Facet -> Model 'Two
  Square :: Double -> Bool -> Model 'Two
  Rectangle :: V2 Double -> Bool -> Model 'Two
  Polygon :: [V2 Double] -> [[Int]] -> Maybe Int -> Model 'Two

  Sphere :: Double -> Facet -> Model 'Three
  Cube :: Double -> Bool -> Model 'Three
  Box :: V3 Double -> Bool -> Model 'Three
  Cylinder :: Double -> Double -> Bool -> Facet -> Model 'Three
  Cylinder2 :: Double -> Double -> Double -> Bool -> Facet -> Model 'Three

  Projection :: Bool -> Model 'Three -> Model 'Two
  OffsetR :: Double -> Bool -> (Model 'Two) -> Model 'Two
  OffsetDelta :: Double -> Bool -> (Model 'Two) -> Model 'Two

  Translate :: (V d Double) -> (Model d) -> Model d
  RotateA :: (V 'Three Radian) -> (Model d) -> Model d
  RotateV :: Radian -> (V 'Three Double) -> (Model d) -> Model d
  Scale :: (V d Double) -> (Model d) -> Model d
  Resize :: (V d Double) -> (Model d) -> Model d
  Mirror :: (V d Double) -> (Model d) -> Model d
  Hull :: [Model d] -> Model d
  Minkowski :: [Model d] -> Model d

  Union' :: [Model d] -> Model d
  Intersection' :: [Model d] -> Model d
  Difference :: Model d -> Model d -> Model d

deriving instance Show (Model 'Two)
deriving instance Show (Model 'Three)


ppFacets :: Facet -> [Doc ann]
ppFacets f =
  catMaybes
    [ (("$fa = " <>) . pretty) <$> _fa f
    , (("$fs = " <>) . pretty) <$> _fs f
    , (("$fn = " <>) . pretty) <$> _fn f
    ]

ppBool :: Bool -> Text
ppBool True = "true"
ppBool False = "false"

block :: (Pretty (Model d)) => [Model d] -> Doc ann
block [Union' xs] = block xs
block xs = vcat [nest 2 (vcat (lbrace : fmap pretty xs)), rbrace]

named :: (Pretty a) => Doc ann -> a -> Doc ann
named n v = n <+> "=" <+> pretty v

named' :: (Pretty a) => Doc ann -> Maybe a -> [Doc ann]
named' _ Nothing = []
named' n (Just v) = [named n v]

center :: Bool -> Doc ann
center = named "center" . ppBool

instance Pretty (Model 'Two) where
  pretty Zero = error "cannot render Zero"
  pretty One = error "cannot render One"

  pretty (Circle r f) = "circle" <> tupled (pretty r : ppFacets f) <> ";"
  pretty (Square r c) = "square" <> tupled (pretty r : [center c]) <> ";"
  pretty (Rectangle r c) = "square" <> tupled (pretty r : [center c]) <> ";"
  pretty (Polygon vs [] c) =
    "polygon" <> tupled (align (list (fmap pretty vs)) : named' "convexity" c) <>
    ";"
  pretty (Polygon vs [p] c) =
    "polygon" <>
    tupled
      (align (list (fmap pretty vs)) : named "paths" p : named' "convexity" c) <>
    ";"
  pretty (Polygon vs ps c) =
    "polygon" <>
    tupled
      (align (list (fmap pretty vs)) : named "paths" ps : named' "convexity" c) <>
    ";"

  pretty (Projection c m) = "projection" <> parens (named "cut" c) <+> block [m]
  pretty (OffsetR r c m) =
    "offset" <> tupled [named "r" r, named "chamfer" (ppBool c)] <+> block [m]
  pretty (OffsetDelta d c m) =
    "offset" <> tupled [named "delta" d, named "chamfer" (ppBool c)] <+>
    block [m]

  pretty (Translate v m) = "translate" <> parens (pretty v) <+> block [m]
  pretty (RotateA a m) = "rotate" <> parens (named "a" a) <+> block [m]
  pretty (RotateV a v m) =
    "rotate" <> parens (list [named "a" a, named "v" v]) <+> block [m]
  pretty (Scale v m) = "scale" <> parens (pretty v) <+> block [m]
  pretty (Resize v m) = "resize" <> parens (pretty v) <+> block [m]
  pretty (Mirror (V2 x y) m) = "mirror" <> parens (pretty (V3 x y 0)) <+> block [m]
  pretty (Hull ms) = "hull()"  <+> block ms
  pretty (Minkowski ms) = "minkowski()"  <+> block ms

  pretty (Union' xs) = "union()" <+> block xs
  pretty (Intersection' xs) = "intersection()" <+> block xs
  pretty (Difference x (Union' ys)) = "difference()" <+> block (x : ys)
  pretty (Difference x y) = "difference()" <+> block [x, y]

instance Pretty (Model 'Three) where
  pretty Zero = error "cannot render Zero"
  pretty One = error "cannot render One"

  pretty (Sphere r f) = "sphere" <> tupled (pretty r : ppFacets f) <> ";"
  pretty (Cube r c) = "cube" <> tupled (pretty r : [center c]) <> ";"
  pretty (Box r c) = "cube" <> tupled (pretty r : [center c]) <> ";"
  pretty (Cylinder h r c f) =
    "cylinder" <> tupled ((named "h" h) : (named "r" r) : center c : ppFacets f) <>
    ";"
  pretty (Cylinder2 h r1 r2 c f) =
    "cylinder" <>
    tupled
      ((named "h" h) : (named "r1" r1) : (named "r2" r2) : center c : ppFacets f) <>
    ";"

  pretty (Translate v m) = "translate" <> parens (pretty v) <+> block [m]
  pretty (RotateA a m) = "rotate" <> parens (named "a" a) <+> block [m]
  pretty (RotateV a v m) =
    "rotate" <> parens (list [named "a" a, named "v" v]) <+> block [m]
  pretty (Scale v m) = "scale" <> parens (pretty v) <+> block [m]
  pretty (Resize v m) = "resize" <> parens (pretty v) <+> block [m]
  pretty (Mirror v m) = "mirror" <> parens (pretty v) <+> block [m]
  pretty (Hull ms) = "hull()"  <+> block ms
  pretty (Minkowski ms) = "minkowski()"  <+> block ms

  pretty (Union' xs) = "union()" <+> block xs
  pretty (Intersection' xs) = "intersection()" <+> block xs
  pretty (Difference x (Union' ys)) = "difference()" <+> block (x : ys)
  pretty (Difference x y) = "difference()" <+> block [x, y]
