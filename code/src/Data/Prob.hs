{-# LANGUAGE ConstraintKinds #-}

module Data.Prob
    ( Prob
    , fromDouble
    ) where

type Prob p = (Ord p, Fractional p, Real p)

fromDouble :: (Fractional a, Real a) => Double -> a
fromDouble = fromRational . toRational
