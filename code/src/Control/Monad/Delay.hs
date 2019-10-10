module Control.Monad.Delay
    ( module Control.Monad.Delay.Class
    , module Control.Monad.Delay.DelayT
    , uniform
    ) where

import Control.Monad.Delay.Class
import Control.Monad.Delay.DelayT

uniform :: Ord t => MonadDelay p t m => t -> t -> Int -> m ()
uniform a b n
    | b < a     = absurd
    | b == a    = delay a
    | otherwise = do
        let d = (b - a) / fromIntegral n
        t <- pick $ a :| [a + d * fromIntegral i | i <- [1 .. n]]
        delay t 
