{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Control.Monad.Probability.Sample
    ( ProbS (..)
    ) where

import Control.Monad.Probability.Class
import Control.Monad.Random (MonadRandom (..))

newtype ProbS p m a = PS {runProbS :: m a }
    deriving (Functor, Applicative, Monad, MonadRandom)

instance (Prob p, MonadRandom m) => MonadProb p (ProbS p m) where
    coin p = do
        x <- fromDouble <$> getRandomR (0, 1)
        return $ x <= p
