{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( someFunc
    , model
    ) where

import Protolude hiding (rotate)

import Graphics.Scad
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutSmart)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc, renderIO)

d2r :: Double -> Double
d2r d = pi * d / 180

model :: Form'
model =
  fn 90 $
  (cylinder 20 5 <->
   rotate'   (V3 0 (d2r 140) (d2r (-45)))                        (cylinder 25 2) <->
   rotate'   (V3 0 (d2r 40) (d2r (-50)))                         (cylinder 30 2) <->
   translate (V3 0 0 (-10)) (rotate' (V3 0 (d2r 40) (d2r (-50))) (cylinder 30 1.4))
  )


someFunc :: IO ()
someFunc = do
  putDoc (render model)
  putStrLn ""
  withFile "test.scad" WriteMode $ \h ->
    renderIO h . layoutSmart defaultLayoutOptions $ render model
