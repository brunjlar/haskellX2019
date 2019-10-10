{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Control.Monad.Delay.DelayT
    ( DelayT (..)
    ) where

import Control.Monad.Delay.Class
import Control.Monad.Except
import Control.Monad.Writer.Strict
import Data.Monoid (Sum (..))

newtype DelayT p t m a = DT (WriterT (Sum t) (ExceptT () m) a)
    deriving (Functor, Applicative, Monad, MonadError (), MonadWriter (Sum t))

instance Time t => MonadTrans (DelayT p t) where
    lift = DT . lift . lift

instance (Time t, MonadProb p m) => MonadProb p (DelayT p t m) where
    coin = lift . coin

instance (Time t, MonadProb p m) => MonadDelay p t (DelayT p t m) where
    delay = tell . Sum
