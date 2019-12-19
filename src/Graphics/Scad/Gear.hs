{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Graphics.Scad.Gear
  ( Involute(..)
  , Planetary(..)
  , tooth
  , gear
  , planetary
  ) where
import Graphics.Scad

import Data.Map (Map)
import Data.Text (Text)
import Polysemy
import Polysemy.Reader
import Polysemy.State

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

invol :: Double -> Double
invol alpha = tan alpha - alpha

baseRadius :: Involute -> Double -> Double
baseRadius i rPitch = cos (pressureAngle i) * rPitch

involute :: Involute -> Double -> [V2 Double]
involute i rPitch =
  let rBase = baseRadius i rPitch
      minR = rBase
      maxR = rPitch + addendum i
      dR = (maxR - minR) / fromIntegral (nSegment i)
      rs = [minR + fromIntegral j * dR | j <- [0 .. nSegment i]]
      as = [acos (rBase / r) | r <- rs]
      xs = [r * cos (invol a) | (r, a) <- zip rs as]
      ys = [r * sin (invol a) | (r, a) <- zip rs as]
   in zipWith V2 xs ys

tooth ::
     (Member (Reader Facet) r, Member (State (Map SomeModel Text)) r)
  => Involute
  -> Double
  -> Sem r Shape
tooth i rPitch =
  let rBase = baseRadius i rPitch
      alphaRef = invol (acos (rBase / rPitch))
      c = convex (involute i rPitch)
      c' = rotate2d (-alphaRef) c
      pitch = pi * module' i
      theta = pitch / rPitch / 2
   in smodule (hull [children, rotate2d theta $ mirror (V2 0 1) children]) c'

gear ::
     (Member (Reader Facet) r, Member (State (Map SomeModel Text)) r)
  => Involute
  -> Double
  -> Sem r Shape
gear i rPitch =
  let pitch = pi * module' i
      theta = pitch / rPitch
      t = tooth i rPitch
   in smodule
        (union
           (circle (rPitch - dedendum i) :
            [ rotate2d (theta * n) children
            | n <- [0 .. 2 * rPitch / module' i - 1]
            ]))
        t

planetary ::
     (Member (Reader Facet) r, Member (State (Map SomeModel Text)) r)
  => Involute
  -> Planetary
  -> Double
  -> Sem r Form
planetary i p height =
  let sun = gear i (rSun p)
      parity = 0.5 * fromIntegral (round (2 * rPlanet p / module' i) `mod` 2 :: Int)
      i' = i {addendum = dedendum i, dedendum = addendum i}
      rRing = rSun p + 2 * rPlanet p
      ring = circle (rOuter p) <-> gear i' rRing
      pitch = pi * module' i
            -- beta: the angle of omega per tooth cycle
      betaRing = pitch / rRing
      betaSun = pitch / rSun p -- betaPlanet == betaSun since meshed
      dRing_dOmega = 1 / betaRing
      dSun_dOmega = 1 / betaSun
      theta = pitch / rPlanet p
      planet omega =
        let (_ :: Int, phaseRing) = properFraction (omega / betaRing)
            (_ :: Int, phasePlnt) = properFraction (parity - omega / betaSun)
            phasePlnt' =
              if phasePlnt < 0
                then phasePlnt + 1
                else phasePlnt
            delta = (phasePlnt' - phaseRing) / (dRing_dOmega + dSun_dOmega)
            omega' = omega + 1 * delta
         in rotate' (V3 0 0 omega') .
            translate (V3 (rPlanet p + rSun p) 0 0) .
            herringbone (-1) (rPlanet p) .
            rotate2d ((theta / 2) + (omega' * rSun p / rPlanet p)) $
            children
   in union
        (herringbone (-1) rRing ring :
         herringbone (1) (rSun p) sun :
         [ smodule
             (union
                [ planet (tau / fromIntegral (nPlanet p) * fromIntegral n)
                | n <- [0 .. nPlanet p - 1]
                ])
             (mirror (V2 1 0) . offsetR (-planetOffset p) False $
              gear i (rPlanet p))
         ])
  where
    herringbone ::
         (Member (Reader Facet) r, Member (State (Map SomeModel Text)) r)
      => Double
      -> Double
      -> Sem r Shape
      -> Sem r Form
    herringbone sgn r m =
      let eps = 1e-5
          height' = height + eps
       in smodule (children <+> mirror (V3 0 0 1) children) .
          translate (V3 0 0 (-eps / 2)) $
          linearExtrude (0.5 * height') False 10 (0.5 * sgn * height' / r) m
