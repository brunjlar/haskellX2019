module Control.Monad.Race
    ( module Control.Monad.Delay
    , module Control.Monad.Race.Class
    , module Control.Monad.Race.Discrete
    , ftf
    , ltf
    ) where

import Control.Monad.Delay
import Control.Monad.Race.Class
import Control.Monad.Race.Discrete

ftf :: MonadRace p t m => [m a] -> m a
ftf []         = absurd
ftf (ma : mas) = either fst snd <$> race ma (ftf mas)

ltf :: MonadRace p t m => [m a] -> m [a]
ltf []         = return []
ltf (ma : mas) = do
    e <- race ma $ ltf mas
    case e of
        Left (a, m)   -> (a :) <$> m
        Right (m, xs) -> m >>= \a -> return (a : xs)
