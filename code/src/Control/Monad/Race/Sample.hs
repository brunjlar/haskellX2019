{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Monad.Race.Sample
    ( RaceS (..)
    ) where

import Control.Monad
import Control.Monad.Race.Class
import Control.Monad.Random (MonadRandom (..))

newtype RaceS p t m a = RS {runRS :: m (Maybe (a, t))}
    deriving Functor

instance (Time t, MonadRandom m) => Applicative (RaceS p t m) where
    pure = return
    (<*>) = ap

instance (Time t, MonadRandom m) => Monad (RaceS p t m) where
    RS x >>= cont = RS $ do
        mat <- x
        case mat of
            Nothing     -> return Nothing
            Just (a, t) -> do
                mbs <- runRS $ cont a
                case mbs of
                    Nothing     -> return Nothing
                    Just (b, s) -> return $ Just (b, t + s)

instance (Time t, MonadRandom m) => MonadError () (RaceS p t m) where

    throwError () = RS $ return Nothing

    catchError (RS x) h = RS $ do
        mat <- x
        case mat of
            Nothing  -> runRS $ h ()
            Just _   -> return mat

instance (Prob p, Time t, MonadRandom m) => MonadProb p (RaceS p t m) where
    coin p = RS $ do
        x <- fromDouble <$> getRandomR (0, 1)
        return $ Just (x <= p, 0)

instance (Prob p, Time t, MonadRandom m) => MonadDelay p t (RaceS p t m) where
    delay t = RS $ return $ Just ((), t)    

instance (Prob p, Time t, MonadRandom m) => MonadRace p t (RaceS p t m) where
    race (RS x) (RS y) = RS $ do
        mat <- x
        mbs <- y
        case (mat, mbs) of
            (Nothing,     Nothing)     -> return Nothing
            (Just (a, t), Nothing)     -> return $ Just (Left (a, absurd), t)
            (Nothing,     Just (b, s)) -> return $ Just (Right (absurd, b), s)
            (Just (a, t), Just (b, s))
                | t <= s               -> return $ Just (Left (a, delay (s - t) >> return b), t)
                | otherwise            -> return $ Just (Right (delay (t - s) >> return a, b), s)

