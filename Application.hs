module Application (
    -- * types
    Params (..),
    Minutes (..),
    -- * functions
    fromParams,
    toMinutes,
    module Model
    ) where

import System.Random (newStdGen,randomRs,randomRIO,Random, randoms)
import Control.Monad (forM)
import Data.List (unfoldr)

-- local
import Model 

-- | all times in seconds, variances
data Params = Params {
    peopleDistr :: StopName -> Time -> (Time,Time), -- ^ valid random range indexed by 'StopName' and 'Time'
    stopsDef :: [(StopName,Time)], -- ^ for each stop, name and driving time distance to the next
    tramsCapacity :: Count, -- ^ number of people on each tram
    boardingDelay :: Count -> Time, -- ^ boarding/unboarding time for a set of people
    tramSource :: (StopName,Time) -- ^ generating tram stop with frequency
    }

-- stream of times following a time parametrized flat distribution
peopleArrivals :: (Time -> (Time, Time)) -> IO [Time]
peopleArrivals f = do
    let g (t, r:rs) = Just (t',(t',rs)) where
            t' = t + t0 + mod r (t1 - t0 + 1)
            (t0,t1) = f t
    unfoldr g <$> (,) 0 <$> randoms <$> newStdGen

-- | prepare the simulation
fromParams :: Params -> IO [Log Time]
fromParams (Params pd ss tc bd (capo,fr)) = do
    let names = map fst ss
    sc <- forM ss $ \(sn,d) -> do
        ps <- peopleArrivals (pd sn)
        ns <- filter (/= sn) <$> map (names !!) <$> randomRs (0,length names - 1) <$> newStdGen
        let mh = case sn == capo of
                    False ->  Nothing
                    True -> Just ([TramId x | x <- [1..]],fr)
        return $ StopConfig sn (zip ps ns) d mh
    return $ simulation (Controls bd tc) sc
     
-- | expressing time in minutes
data Minutes = Minutes Int Int

instance Show Minutes where
    show (Minutes n s) = show n ++ "'" ++ show s ++"\""

-- | convert from time to 'Minutes'
toMinutes :: Time -> Minutes
toMinutes  = uncurry Minutes . (`divMod` 60)

