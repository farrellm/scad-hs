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
  , OffsetMode(..)
  , Model(..)
  , Module(..)
  , SomeModel(..)
  , SomeModels(..)
  , IsSomeModel(..)
  ) where

import Graphics.Scad.Orphan ()

import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Data.Maybe (catMaybes)
import Data.Text.Prettyprint.Doc
import Data.Text (Text)

newtype Radian = Radian Double
  deriving (Show, Eq, Ord)

instance Pretty Radian where
  pretty (Radian r) = pretty (180 * r / pi)

data Facet =
  Facet
    { _fa :: Maybe Double
    , _fs :: Maybe Double
    , _fn :: Maybe Double
    }
  deriving (Show, Eq, Ord)

data Dimension = Two | Three
  deriving (Show, Eq, Ord)

type family V d where
  V 'Two = V2
  V 'Three = V3

data OffsetMode
  = OffsetR Double
  | OffsetDelta Double
  deriving (Show, Eq, Ord)

data Model d where
  Circle :: Double -> Facet -> Model 'Two
  Square :: Double -> Bool -> Model 'Two
  Rectangle :: V2 Double -> Bool -> Model 'Two
  Polygon :: [V2 Double] -> [[Int]] -> Maybe Int -> Model 'Two

  Sphere :: Double -> Facet -> Model 'Three
  Cube :: Double -> Bool -> Model 'Three
  Box :: V3 Double -> Bool -> Model 'Three
  Cylinder :: Double -> Double -> Bool -> Facet -> Model 'Three
  Cylinder2 :: Double -> Double -> Double -> Bool -> Facet -> Model 'Three
  LinearExtrude :: Double -> Bool -> Int -> Radian -> Facet -> Model 'Two -> Model 'Three

  Projection :: Bool -> Model 'Three -> Model 'Two
  Offset :: OffsetMode -> Bool -> Model 'Two -> Model 'Two

  Translate :: V d Double -> Model d -> Model d
  RotateA :: V 'Three Radian -> Model d -> Model d
  RotateV :: Radian -> V 'Three Double -> (Model d) -> Model d
  Scale :: V d Double -> Model d -> Model d
  Resize :: V d Double -> Model d -> Model d
  Mirror :: V d Double -> Model d -> Model d
  Hull :: [Model d] -> Model d
  Minkowski :: [Model d] -> Model d

  Union' :: [Model d] -> Model d
  Intersection' :: [Model d] -> Model d
  Difference :: Model d -> Model d -> Model d

  Apply2 :: Text -> Model 'Two -> Model d
  Apply3 :: Text -> Model 'Three -> Model d
  Children :: Model d

deriving instance Show (Model 'Two)
deriving instance Show (Model 'Three)

deriving instance Eq (Model 'Two)
deriving instance Eq (Model 'Three)

deriving instance Ord (Model 'Two)
deriving instance Ord (Model 'Three)

data Module where
  Module2 :: Text -> [Model 'Two] -> Module
  Module3 :: Text -> [Model 'Three] -> Module
  deriving (Show, Eq, Ord)

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
block xs = vcat [nest 2 (vcat (lbrace : fmap pretty xs)), rbrace]

named :: (Pretty a) => Doc ann -> a -> Doc ann
named n v = n <+> "=" <+> pretty v

named' :: (Pretty a) => Doc ann -> Maybe a -> [Doc ann]
named' _ Nothing = []
named' n (Just v) = [named n v]

center :: Bool -> Doc ann
center = named "center" . ppBool

instance Pretty (Model 'Two) where
  pretty (Circle r f) =
    "circle" <> align (tupled (pretty r : ppFacets f)) <> ";"
  pretty (Square r c) =
    "square" <> align (tupled (pretty r : [center c])) <> ";"
  pretty (Rectangle r c) =
    "square" <> align (tupled (pretty r : [center c])) <> ";"
  pretty (Polygon vs [] c) =
    "polygon" <>
    align (tupled (align (list (fmap pretty vs)) : named' "convexity" c) <> ";")
  pretty (Polygon vs [p] c) =
    "polygon" <>
    align
      (tupled
         (align (list (fmap pretty vs)) : named "paths" p : named' "convexity" c) <>
       ";")
  pretty (Polygon vs ps c) =
    "polygon" <>
    tupled
      (align (list (fmap pretty vs)) : named "paths" ps : named' "convexity" c) <>
    ";"

  pretty (Projection c m) = "projection" <> parens (named "cut" c) <+> block [m]
  pretty (Offset (OffsetR r) c m) =
    "offset" <> align (tupled [named "r" r, named "chamfer" (ppBool c)]) <+>
    block [m]
  pretty (Offset (OffsetDelta d) c m) =
    "offset" <> align (tupled [named "delta" d, named "chamfer" (ppBool c)]) <+>
    block [m]

  pretty (Translate v m) = "translate" <> parens (pretty v) <+> block [m]
  pretty (RotateA a m) = "rotate" <> parens (named "a" a) <+> block [m]
  pretty (RotateV a v m) =
    "rotate" <> parens (list [named "a" a, named "v" v]) <+> block [m]
  pretty (Scale v m) = "scale" <> parens (pretty v) <+> block [m]
  pretty (Resize v m) = "resize" <> parens (pretty v) <+> block [m]
  pretty (Mirror (V2 x y) m) =
    "mirror" <> parens (pretty (V3 x y 0)) <+> block [m]
  pretty (Hull ms) = "hull()" <+> block ms
  pretty (Minkowski ms) = "minkowski()" <+> block ms

  pretty (Union' xs) = "union()" <+> block xs
  pretty (Intersection' xs) = "intersection()" <+> block xs
  pretty (Difference x (Union' ys)) = "difference()" <+> block (x : ys)
  pretty (Difference x y) = "difference()" <+> block [x, y]

  pretty (Apply2 n m) = pretty n <> "()" <+> block [m]
  pretty (Apply3 n m) = pretty n <> "()" <+> block [m]
  pretty Children = "children();"


instance Pretty (Model 'Three) where
  pretty (Sphere r f) =
    "sphere" <> align (tupled (pretty r : ppFacets f)) <> ";"
  pretty (Cube r c) = "cube" <> align (tupled (pretty r : [center c])) <> ";"
  pretty (Box r c) = "cube" <> align (tupled (pretty r : [center c])) <> ";"
  pretty (Cylinder h r c f) =
    "cylinder" <>
    align
      (tupled ((named "h" h) : (named "r" r) : center c : ppFacets f) <> ";")
  pretty (Cylinder2 h r1 r2 c f) =
    "cylinder" <>
    align
      (tupled
         ((named "h" h) :
          (named "r1" r1) : (named "r2" r2) : center c : ppFacets f) <>
       ";")

  pretty (LinearExtrude h c v t f m) =
    "linear_extrude" <>
    align
      (tupled
         (named "height" h :
          named "center" (ppBool c) :
          named "convexity" v : named "twist" t : ppFacets f)) <+>
    block [m]

  pretty (Translate v m) = "translate" <> parens (pretty v) <+> block [m]
  pretty (RotateA a m) = "rotate" <> parens (named "a" a) <+> block [m]
  pretty (RotateV a v m) =
    "rotate" <> tupled [named "a" a, named "v" v] <+> block [m]
  pretty (Scale v m) = "scale" <> parens (pretty v) <+> block [m]
  pretty (Resize v m) = "resize" <> parens (pretty v) <+> block [m]
  pretty (Mirror v m) = "mirror" <> parens (pretty v) <+> block [m]
  pretty (Hull ms) = "hull()" <+> block ms
  pretty (Minkowski ms) = "minkowski()" <+> block ms

  pretty (Union' xs) = "union()" <+> block xs
  pretty (Intersection' xs) = "intersection()" <+> block xs
  pretty (Difference x (Union' ys)) = "difference()" <+> block (x : ys)
  pretty (Difference x y) = "difference()" <+> block [x, y]

  pretty (Apply2 n m) = pretty n <> "()" <+> block [m]
  pretty (Apply3 n m) = pretty n <> "()" <+> block [m]
  pretty Children = "children();"


instance Pretty Module where
  pretty (Module2 n m) = "module" <+> pretty n <> "()" <+> block m
  pretty (Module3 n m) = "module" <+> pretty n <> "()" <+> block m


data SomeModel
  = Model2 (Model 'Two)
  | Model3 (Model 'Three)
  deriving (Show, Eq, Ord)

data SomeModels
  = Models2 [Model 'Two]
  | Models3 [Model 'Three]
  deriving (Show, Eq, Ord)

class IsSomeModel a where
  someModel :: a -> SomeModel
  someModels :: [a] -> SomeModels

instance IsSomeModel (Model 'Two) where
  someModel = Model2
  someModels = Models2

instance IsSomeModel (Model 'Three) where
  someModel = Model3
  someModels = Models3
