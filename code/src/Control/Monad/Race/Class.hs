{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Control.Monad.Race.Class
    ( module Control.Monad.Delay.Class
    , MonadRace (..)
    ) where

import           Control.Monad.Except (MonadError (..))
import           Control.Monad.Delay.Class

class MonadDelay p t m => MonadRace p t m | m -> p t where
    race  :: m a -> m b -> m (Either (a, m b) (m a, b))
