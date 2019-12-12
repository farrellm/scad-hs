{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.Scad.Orphan where

import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Data.Text.Prettyprint.Doc

instance Pretty a => Pretty (V2 a) where
  pretty (V2 x y) = list (pretty <$> [x, y])

instance Pretty a => Pretty (V3 a) where
  pretty (V3 x y z) = list (pretty <$> [x, y, z])
