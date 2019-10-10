{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Probability.List
    ( ProbL (..)
    , probLOrd
    ) where

import           Control.Monad
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.Probability.Class

newtype ProbL p a = PL {runProbL :: [(a, p)]}
    deriving (Show, Read, Eq, Ord, Functor)

instance Prob p => Applicative (ProbL p) where
    pure = return
    (<*>) = ap

instance Prob p => Monad (ProbL p) where

    return a = PL [(a, 1)]

    PL aps >>= cont = PL $ do
        (a, p) <- aps
        (b, q) <- runProbL $ cont a
        return (b, p * q)

instance Prob p => MonadProb p (ProbL p) where
    coin p
        | p <= 0    = return False
        | p >= 1    = return True
        | otherwise = PL [(True, p), (False, 1 - p)]

probLOrd :: (Prob p, Ord a) => ProbL p a -> Map a p
probLOrd = foldl' f Map.empty . runProbL
  where
    f m (a, p) = Map.insertWith (+) a p m
