{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Control.Monad.Delay.Class
    ( module Control.Monad.Probability.Class
    , MonadError (..)
    , Time
    , MonadDelay (..)
    , absurd
    , catch
    ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Probability.Class

type Time t = (Fractional t, Ord t)

class ( MonadError () m
      , MonadProb p m
      , Time t
      ) => MonadDelay p t m | m -> p t where
    delay :: t -> m ()

absurd :: MonadDelay p t m => m a
absurd = throwError ()

catch :: MonadDelay p t m => m a -> m a -> m a
catch m h = catchError m $ const h
