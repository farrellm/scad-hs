module Graphics.Scad.Gear where

import Graphics.Scad
import Graphics.Scad.Types

import Debug.Trace

data Involute a =
  Involute
    { pressureAngle :: a
    , pitchRadius :: a
    , addendum :: a
    , dedendum :: a
    , nSegment :: Int
    }
  deriving (Show)

test :: Involute Double
test =
  Involute
    { pressureAngle = pi * 20 / 180
    , pitchRadius = 75
    , addendum = 5
    , dedendum = 3
    , nSegment = 4
    }

invol :: Floating a => a -> a
invol alpha = tan alpha - alpha

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 [] _ _ _ = []
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds

involute :: (Show a, Floating a) => Involute a -> [(a, a)] -- Shape
involute i =
  let rb = cos (pressureAngle i) * pitchRadius i
      minR = pitchRadius i - dedendum i
      maxR = pitchRadius i + addendum i
      dR = (maxR - minR) / fromIntegral (nSegment i)
      rs = [minR + fromIntegral i * dR | i <- [0 .. nSegment i]]
      as = [acos (rb / r) | r <- rs]
      xs = [r * cos (invol a) | (r, a) <- zip rs as]
      ys = [r * sin (invol a) | (r, a) <- zip rs as]
   in traceShow rb $ traceShow rs $ traceShow (zip4 rs (fmap (\r -> 180 * r / pi) as) xs ys) $ []
