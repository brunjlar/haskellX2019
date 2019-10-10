{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Monad.Race.Discrete
    ( RaceL (..)
    , raceOrd
    , plot
    ) where

import           Control.Lens ((.~))
import           Control.Monad
import           Control.Monad.Race.Class
import           Data.Colour.Names (red)
import           Data.Default.Class
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.Chart as C
import qualified Graphics.Rendering.Chart.Backend.Cairo as C
import qualified Graphics.Rendering.Chart.Easy as C
import           Text.Printf (printf)

newtype RaceL p t a = RL {runRL :: [(t, p, a)]}
    deriving (Show, Read, Eq, Ord, Functor)

instance (Prob p, Time t) => Applicative (RaceL p t) where
    pure = return
    (<*>) = ap

instance (Prob p, Time t) => Monad (RaceL p t) where
    return a = RL [(0, 1, a)]
    RL xs >>= cont = RL $ do
        (ta, pa, a) <- xs
        (tb, pb, b) <- runRL $ cont a
        return (ta + tb, pa * pb, b)

instance (Prob p, Time t) => MonadError () (RaceL p t) where
    throwError () = RL []
    catchError (RL [])  h = h ()
    catchError m        _ = m

instance (Prob p, Time t) => MonadProb p (RaceL p t) where
    coin p
        | p <= 0    = return False
        | p >= 1    = return True
        | otherwise = RL [(0, p, True), (0, 1 - p, False)]

instance (Prob p, Time t) => MonadDelay p t (RaceL p t) where
    delay t = RL [(t, 1, ())]

instance (Prob p, Time t) => MonadRace p t (RaceL p t) where
    race (RL xs) (RL ys) = RL $ do
        let fx  = max 0 $ min 1 $ 1 - weight xs
            fy  = max 0 $ min 1 $ 1 - weight ys
            za  = if fy > 0 then [(ta, pa * fy, Left (a, absurd)) | (ta, pa, a) <- xs] else []
            zb  = if fx > 0 then [(tb, pb * fx, Right (absurd, b)) | (tb, pb, b) <- ys] else []
            zab = do
                (ta, pa, a) <- xs
                (tb, pb, b) <- ys
                return $ if ta <= tb
                    then (ta, pa * pb, Left (a, RL [(tb - ta, 1, b)]))
                    else (tb, pa * pb, Right (RL [(ta - tb, 1, a)], b))
        za ++ zb ++ zab
      where
        weight :: [(t, p, c)] -> p
        weight ws = sum [p | (_, p, _) <- ws]

raceOrd :: forall p t a. (Prob p, Time t, Ord a)
        => RaceL p t a
        -> Map a (Map t p)
raceOrd (RL xs) = foldl' f Map.empty xs
  where
    f :: Map a (Map t p) -> (t, p, a) -> Map a (Map t p)
    f m (t, p, a) = flip (Map.insert a) m $ case Map.lookup a m of
        Nothing -> Map.singleton t p
        Just m' -> Map.insertWith (+) t p m'

plot :: String -> FilePath -> RaceL Rational Rational () -> IO ()
plot title fp x = case Map.lookup () $ raceOrd x of
    Nothing -> putStrLn "absurd"
    Just m  -> do
        let o = C.fo_format .~ C.SVG $ def
        void $ C.renderableToFile o fp $ C.layoutToRenderable $ layout m
  where
    layout :: Map Rational Rational -> C.Layout Double Double
    layout m = C.layout_plots .~ [C.toPlot $ points m]
                 $ C.layout_title .~ printf
                      "%s: weight %6.4f, mean %8.4f"
                      title
                      (fromRational $ sum m :: Double)
                      mean
                 $ C.layout_x_axis . C.laxis_title .~ "time"
                 $ C.layout_y_axis . C.laxis_title .~ "probability"
                 $ def
      where
        weight :: Rational
        weight = sum m
         
        mean :: Double
        mean = fromRational (foldl' f 0 (Map.toList m) / weight)

        f :: Rational -> (Rational, Rational) -> Rational
        f !s (t, p) = s + t * p

    points :: Map Rational Rational -> C.PlotPoints Double Double
    points m = C.plot_points_values .~ [ (fromRational t, fromRational p)
                                       | (t, p) <- Map.toList m
                                       ]
             $ C.plot_points_style . C.point_shape .~ C.PointShapeCircle
             $ C.plot_points_style . C.point_radius .~ 5
             $ C.plot_points_style . C.point_color .~ C.opaque red
             $ def
