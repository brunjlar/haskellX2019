module Examples.Dice
    ( die
    , dice
    , diceS
    , diceL
    ) where

import qualified Data.Map.Strict as Map
import           Control.Monad (replicateM)
import           Control.Monad.Probability

die :: MonadProb p m => m Int
die = pick $ 1 :| [2 .. 6]

dice :: MonadProb p m => Int -> m Int
dice n
    | n <= 0    = return 0
    | otherwise = sum <$> replicateM n die

diceS :: Int -> IO [Int]
diceS c = runProbS $ replicateM c (dice 2 :: ProbS Double IO Int)

diceL :: [(Int, Rational)]
diceL = Map.toList $ probLOrd $ dice 2
