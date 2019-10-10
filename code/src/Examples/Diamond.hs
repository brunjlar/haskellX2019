{-# LANGUAGE FlexibleContexts #-}

module Examples.Diamond where

import Control.Monad.Race

-- one edge
d :: (Ord t, Num t, MonadRace p t m) => m ()
d = do
        b <- coin 0.9
        if b then uniform 1 2 20 else absurd

--    2
--   / \
--  /   \
-- 1     4
--  \   /
--   \ /
--    3

diamond1 :: MonadRace p t m => m ()
diamond1 = do
    let d12 = d; d13 = d; d24 = d; d34 = d
    ftf [d12 >> d24, d13 >> d34]

--    2
--   /|\
--  / | \
-- 1  |  4
--  \ | /
--   \V/
--    3

diamond2 :: MonadRace p t m => m ()
diamond2 = do
    let d12 = d; d13 = d; d23 = d; d24 = d; d34 = d
    e <- race d12 d13
    case e of
        Left ((), r13)   -> ftf [d24, ftf [d23, r13] >> d34]
        Right (r12, ())  -> ftf [d34, r12 >> d24]

plotDiamond1 :: IO ()
plotDiamond1 = do
    plot "Diamond 1" "diamond1.svg" diamond1

plotDiamond2 :: IO ()
plotDiamond2 = do
    plot "Diamond 2" "diamond2.svg" diamond2
