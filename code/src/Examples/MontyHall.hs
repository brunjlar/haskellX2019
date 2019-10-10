module Examples.MontyHall
    ( Prize (..)
    , Strategy (..)
    , monty
    , montyS
    , montyL
    ) where

import           Control.Monad (replicateM)
import           Control.Monad.Probability
import           Data.List.NonEmpty hiding (head)
import qualified Data.Map.Strict as Map

import Prelude hiding (filter, (!!))

data Prize = Goat | Car deriving (Show, Read, Eq, Ord)

data Strategy = Stay | Change deriving (Show, Read, Eq, Ord)

monty :: MonadProb p m => Strategy -> m Prize
monty s = do
    let doors   = 0 :| [1, 2]
    carDoor     <- pick doors
    let prizes  = (\i -> if i == carDoor then Car else Goat) <$> doors
    guess       <- pick doors
    openedDoor  <- unsafePick $ filter (\i -> i /= guess && prizes !! i == Goat) doors
    let guess' = case s of
            Stay    -> guess
            Change  -> head $ filter (\i -> i /= guess && i /= openedDoor) doors
    return $ prizes !! guess'

montyS :: Int -> Strategy -> IO [Prize]
montyS c s = runProbS $ replicateM c (monty s :: ProbS Double IO Prize)

montyL :: Strategy -> [(Prize, Rational)]
montyL = Map.toList . probLOrd . monty
