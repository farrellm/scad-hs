{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Graphics.Scad.Gear
  ( Involute(..)
  , Planetary(..)
  , tooth
  , gear
  , planetary
  ) where
import Graphics.Scad

import Control.Monad.Reader
import Debug.Trace

data Involute =
  Involute
    { pressureAngle :: Double
    , module' :: Double
    , addendum :: Double
    , dedendum :: Double
    , nSegment :: Int
    }
  deriving (Show)

data Planetary =
  Planetary
    { rOuter :: Double
    , rSun :: Double
    , rPlanet :: Double
    , nPlanet :: Int
    , planetOffset :: Double
    }
  deriving (Show)

-- infixl 7 `rmod`
-- rmod :: (RealFrac a) => a -> a -> a
-- rmod n d =
--   let (_ :: Int, f) = properFraction (n / d)
--    in if f >= 0
--         then f * d
--         else f * d + d

invol :: Double -> Double
invol alpha = tan alpha - alpha

baseRadius :: Involute -> Double -> Double
baseRadius i rPitch = cos (pressureAngle i) * rPitch

involute :: Involute -> Double -> [V2 Double]
involute i rPitch =
  let rBase = baseRadius i rPitch
      minR = rBase -- rPitch - dedendum i
      maxR = rPitch + addendum i
      dR = (maxR - minR) / fromIntegral (nSegment i)
      rs = [minR + fromIntegral j * dR | j <- [0 .. nSegment i]]
      as = [acos (rBase / r) | r <- rs]
      xs = [r * cos (invol a) | (r, a) <- zip rs as]
      ys = [r * sin (invol a) | (r, a) <- zip rs as]
   in zipWith V2 xs ys

tooth :: (Applicative m) => Involute -> Double -> m Shape
tooth i rPitch =
  let rBase = baseRadius i rPitch
      alphaRef = invol (acos (rBase / rPitch))
      c = convex (involute i rPitch)
      c' = rotate2d (-alphaRef) c
      r = mirror (V2 0 1) c'
      pitch = pi * module' i
      theta = pitch / rPitch / 2
   in hull [c', rotate2d theta r]

gear :: (MonadReader Facet m) => Involute -> Double -> m Shape
gear i rPitch =
  let pitch = pi * module' i
      theta = pitch / rPitch
      t = tooth i rPitch
      ts = [rotate2d (theta * n) t | n <- [0 .. 2 * rPitch / module' i - 1]]
   in union ts <+> circle (rPitch - dedendum i)

r2d :: Double -> Double
r2d r = 360 * r / tau

planetary :: (MonadReader Facet m) => Involute -> Planetary -> Double -> m Form
planetary i p height =
  let rRing = rSun p + 2 * rPlanet p
      pitch = pi * module' i
      -- beta: the angle of omega per tooth cycle
      betaRing = pitch / rRing
      betaSun = pitch / rSun p -- betaPlanet == betaSun since meshed
      dRing_dOmega = 1 / betaRing
      dSun_dOmega = 1 / betaSun
      theta = pitch / rPlanet p
      sun = gear i (rSun p)
      parity =
        if round (2 * rPlanet p / module' i) `mod` 2 == 0
          then 0
          else 0.5
      planet =
        \omega ->
          let (_ :: Int, phaseRing) = properFraction (omega / betaRing)
              (_ :: Int, phasePlnt) = properFraction (parity - omega / betaSun)
              phasePlnt' =
                if phasePlnt < 0
                  then phasePlnt + 1
                  else phasePlnt
              delta =
                (1) * (phasePlnt' - phaseRing) / (dRing_dOmega + dSun_dOmega)
              omega' = omega + 1 * delta
           in traceShow
                (r2d omega, phaseRing, phasePlnt', r2d delta, r2d omega') $
              rotate' (V3 0 0 omega') .
              translate (V3 (rPlanet p + rSun p) 0 0) .
              linearExtrude height False 10 (-height / rPlanet p) .
              rotate2d ((theta / 2) + (omega' * rSun p / rPlanet p)) .
              mirror (V2 1 0) $
              offsetR (- planetOffset p) False $
              gear i (rPlanet p)
      i' = i {addendum = dedendum i, dedendum = addendum i}
      ring = circle (rOuter p) <-> gear i' rRing
      half =
        union
          (linearExtrude height False 10 (-height / rRing) ring :
           linearExtrude height False 10 (height / rSun p) sun :
           [ planet (tau / fromIntegral (nPlanet p) * fromIntegral n)
           | n <- [0 .. (nPlanet p) - 1]
           ])
   in half <+> mirror (V3 0 0 1) half
