{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Control.Monad.Probability.Class
    ( module Data.Prob
    , NonEmpty (..)
    , MonadProb (..)
    , unsafePick
    ) where

import           Data.List.NonEmpty (NonEmpty (..), fromList)
import           Data.Prob

class (Prob p, Monad m) => MonadProb p m | m -> p where

    coin :: p -> m Bool

    pick :: NonEmpty a -> m a
    pick (x :| [])     = return x
    pick (x :| y : ys) = do
        let len = length ys
            p   = recip $ fromIntegral $ len + 2
        b <- coin p
        if b then return x
             else pick $ y :| ys

unsafePick :: MonadProb p m => [a] -> m a
unsafePick = pick . fromList
