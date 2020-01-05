{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module ScadTest where

import Protolude hiding (rotate)

import Graphics.Scad
import Graphics.Scad.Gear
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutSmart)
import Data.Text.Prettyprint.Doc.Render.Text -- (renderIO)

d2r :: Double -> Double
d2r d = pi * d / 180

model2 :: Form'
model2 =
  fn 90 $
  (cylinder 20 5 <->
   rotate'   (V3 0 (d2r 140) (d2r (-45)))                        (cylinder 25 2) <->
   rotate'   (V3 0 (d2r 40) (d2r (-50)))                         (cylinder 30 2) <->
   (translate (V3 0 0 (-10)) # (rotate' (V3 0 (d2r 40) (d2r (-50))) (cylinder 30 1.4)))
  )

model7 :: Form'
model7 =
  let mdl = 2.5
      add = 0.68 * mdl -- 1.1
   in slices 10 $
      planetary
        Involute
          { pressureAngle = pi * 45 / 180 -- pi * 20 / 180
          , module' = mdl
          , addendum = add
          , dedendum = add
          , nSegment = 7
          }
        Planetary
          { rOuter = 35
          , rSun = (9 * mdl / 2)
          , rPlanet = (7 * mdl / 2)
          , nPlanet = 5
          , planetOffset = 0.4
          }
        20

model4 :: Form'
model4 =
  let mdl = 2.0
      add = 0.5 * mdl -- 1.1
   in slices 10 $
      planetary
        Involute
          { pressureAngle = pi * 45 / 180 -- pi * 20 / 180
          , module' = mdl
          , addendum = add
          , dedendum = add
          , nSegment = 7
          }
        Planetary
          { rOuter = 19
          , rSun = (7 * mdl / 2)
          , rPlanet = (4 * mdl / 2)
          , nPlanet = 5
          , planetOffset = 0.4
          }
        15

model8 :: Form'
model8 =
  let mdl = 2.5
      add = 1.7
   in planetary
        Involute
          { pressureAngle = pi * 45 / 180 -- pi * 20 / 180
          , module' = mdl
          , addendum = add
          , dedendum = 1.25 * add
          , nSegment = 7
          }
        Planetary
          { rOuter = 33
          , rSun = (8 * mdl / 2)
          , rPlanet = (7 * mdl / 2)
          , nPlanet = 5
          , planetOffset = 0.4
          }
        15

someFunc :: IO ()
someFunc = do
  -- putDoc (render model)
  putDoc (render model2)
  putStrLn ""
  withFile "test.scad" WriteMode $ \h ->
    renderIO h . layoutSmart defaultLayoutOptions $ render model7
