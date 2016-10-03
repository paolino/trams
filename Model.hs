{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses#-}
{-# language FlexibleInstances#-}
{-# language FlexibleContexts#-}
{-# language ViewPatterns#-}
{-# language GeneralizedNewtypeDeriving#-}

-- import qualified Data.Map as M
-- import qualified Data.FingerTree as F

import Data.Map (findWithDefault, delete,insertWith , Map) 
import Data.FingerTree (ViewL((:<)),ViewR((:>)),(<|),(|>),viewl,FingerTree,Measured(..),split,fromList)
import Data.Monoid ((<>))
import Control.Monad (forM)
import Control.Monad.Writer (tell,execWriter,Writer)
import Control.Arrow (second)
import Data.List (unfoldr,mapAccumL)
import Control.Lens hiding ((:<),(:>),(<|),(|>),viewl)
import Control.Lens.TH (makeLenses)
import System.Random (newStdGen,randomRs,randomRIO,Random, randoms)

-- local
import FingerTree (biSelectByMeasure, selectByMeasure, Min (..),swapHead)

-- | Notion of time 
type Time = Int

-- | name of a Stop
newtype StopName = StopName Int deriving (Eq,Show,Ord)

-- counting people
newtype Count = Count Int deriving (Show, Num,Eq, Random, Ord)

-- | partition into destinations of a population
type People = Map StopName Count

-- | comonadic store for people at a stop (miss live randomization ?)
data PeopleStore = PeopleStore {
    update :: Time -> Maybe Count -> ([Person], PeopleStore)
    }

type Person = (Time,StopName)

compress :: [StopName] -> People
compress ds = foldr (uncurry $ insertWith (+)) mempty $ zip ds $ repeat 1

mkPeopleStore :: [Person] -> PeopleStore 
mkPeopleStore = make  where
    u rs t mc = let
        (us,rs') = break ((>t) . fst) rs
        (us',us'') = case mc of
                Just (Count m) -> splitAt m us
                Nothing -> (us,[])
        in (us', make $ us'' ++ rs' )
    make rs = PeopleStore (u rs) 
    
newtype TramId = TramId Int deriving (Show,Eq)

data Tram = Tram {
    tid :: TramId,
    _carrying :: People
    }
    | Fresh {
    tid :: TramId
    }
    deriving Show
makeLenses ''Tram


-- | A scheduled departure time for a tram
data Event = Event Time Tram deriving Show

instance Measured (Min Time) Event where
    measure (Event t _) = Min t

-- | sequence selectable by min time
type Earliers a = FingerTree (Min Time) a

-- | Stop state
data Stop = Stop {
    name :: StopName, -- ^ stop name
    distance :: Time, -- ^ distance to the  next stop
    _waiting :: PeopleStore, -- ^ people at the stop
    _events :: FingerTree (Min Time) Event, -- ^ possible next event
    _pool :: [TramId],
    frequency :: Time
    }
makeLenses ''Stop

instance Measured (Min Time) Stop where
    measure = measure . view events
--------------------------------------------------------------------------------
--------- consuming events ------------------------------------------------------
--------------------------------------------------------------------------------

data Log t 
    = Operating
        TramId
        StopName
        t -- ^ arrival of tram
        Count -- ^ people going down
        [t] -- ^ people waiting times
    | Queued
        TramId
        StopName
        t
    | Leaving 
        TramId
        StopName
        t

    deriving Show

data Controls = Controls {
    speed :: Count -> Time,
    capacity :: Count
    }

movePeople :: Controls -> TramId -> StopName -> Time -> PeopleStore -> People -> Writer [Log Time] (Time, Maybe (PeopleStore, People)) 
movePeople (Controls f cap) tid d t (PeopleStore upd)  p = let
    p' = delete d p -- dematerializing people
    n = findWithDefault 0 d p -- exiting tram count
    (unzip -> (ts,ds), ps') = upd t (Just $ cap - n) -- get most possible people from the stop
    m = max n . Count $ length ts
    w = f m
    in  case m of
            0 ->    do  
                        tell [Leaving tid d t ]
                        return (w, Nothing)
            m ->    do
                        tell [Operating tid d t n(map (flip subtract t) ts)]
                        return (w,Just  (ps',p' <> compress ds))
    
    

consumeEvent :: Controls -> Stop -> Writer [Log Time] (Stop,Maybe (Stop -> Stop))
consumeEvent cs s = let
    es = view events s
    in case selectByMeasure es of
        (e@(Event ti tr), rf) -> do
            -- get the first of the queue
            let h@(Event tif htr) :< rs = viewl es
            case tid htr == tid tr of
                -- if it's not the first in the queue , Just update the scheduling to the departure time of the first (+ something ?)
                False -> do
                    tell [Queued (tid tr) (name s) ti]
                    return (set events (rf . Just $ Event tif tr) $ s, Nothing)
                -- if it's the first check departing conditions
                True -> do
                    case tr of
                        Fresh ntid -> case null rs of
                                -- some are arriving, drop departure
                                False -> return (over events swapHead . set events (rf . Just $ Event (ti + frequency s) tr) $ s,Nothing)
                                -- no trams arriving, enter the game
                                True -> do
                                    -- consume one name from the pool and make it a fresh tram
                                    let (Event (ti + frequency s) . Fresh -> nt) : ps = view pool s
                                    -- queue a real tram and a fresh tram
                                    return (set pool ps . 
                                            set events (fromList [Event ti (Tram ntid mempty),nt]) $ s,
                                        Nothing)
                        _ -> do
                            (w , t) <- movePeople cs (tid tr) (name s) ti (view waiting s) (view carrying tr)
                            case t of
                                -- if they are met leave the queue and transpose as arrival to the next stop
                                Nothing -> return (set events (rf Nothing) $ s, Just $ over events (|> Event (ti + w + distance s) tr))
                                -- if they are not met reschedule for loading more people
                                Just (sp,tp) -> return (set waiting sp . set events (rf . Just $ Event (w + ti) (set carrying tp tr)) $ s, Nothing )

--  circle of stops  
type Track = FingerTree (Min Time) Stop 

-- step the track, logging the boarding
step :: Controls -> Track -> Writer [Log Time] Track
step cs s = corr <$> second (maybe y ($ y)) <$> consumeEvent cs x where
    ((x,y), corr) = biSelectByMeasure s 
    
-- configuration of a stop
data StopConfig = StopConfig 
    StopName -- stop name
    [Person] -- an infinite stream of person boarding
    Time -- distance int seconds to the next stop
    (Maybe ([TramId], Time)) -- queue of departures trams, care! blocking the lane 

-- live stop
mkStop :: StopConfig -> Stop
mkStop (StopConfig name ps d mh) = let 
        c = Stop name d (mkPeopleStore ps) 
        in case mh of
                Nothing -> c mempty [] (error "using a trams we don't have")
                Just (ti:tis,dt) -> c (fromList [Event 0 (Fresh ti)]) tis dt where

-- produce an infinite log from the simulation
program     :: Controls  
            -> [StopConfig] 
            -> [Log Time] -- a stream of simulation states
program cs = execWriter . let f s = step cs s >>= f in f . fromList . map mkStop 

-- all times in seconds, variances
data Params = Params {
    peopleDistr :: StopName -> Time -> (Time,Time),
    stops :: [(StopName,Time)],
    tramsCapacity :: Count,
    boardingDelay :: Count -> Time,
    trams :: (StopName,Time) -- Starting point, freq
    }

peopleArrivals :: (Time -> (Time, Time)) -> IO [Time]
peopleArrivals f = do
    let g (t, r:rs) = Just (t',(t',rs)) where
            t' = t + t0 + mod r (t1 - t0 + 1)
            (t0,t1) = f t
    unfoldr g <$> (,) 0 <$> randoms <$> newStdGen

createSim (Params pd ss tc bd (capo,fr)) = do
    let names = map fst ss
    sc <- forM ss $ \(sn,d) -> do
        ps <- peopleArrivals (pd sn)
        ns <- filter (/= sn) <$> map (names !!) <$> randomRs (0,length names - 1) <$> newStdGen
        let mh = case sn == capo of
                    False ->  Nothing
                    True -> Just ([TramId x | x <- [1..]],fr)
        return $ StopConfig sn (zip ps ns) d mh
    return (Controls bd tc, sc)
                
data Minutes = Minutes Int Int

instance Show Minutes where
    show (Minutes n s) = show n ++ "'" ++ show s ++"\""

fromSeconds = uncurry Minutes . (`divMod` 60)

toMinutes (Operating tis sn t cd ts) = Operating tis sn (fromSeconds t) cd (map fromSeconds ts) 
toMinutes (Queued tis sn t) = Queued tis sn $ fromSeconds t
toMinutes (Leaving tis sn t) = Leaving tis sn $ fromSeconds t


simulate p = uncurry program <$> createSim p 
printLogs n = mapM_ (print . toMinutes) . take n 

-- example 

p0 = Params (\_ _ -> (10,20)) [(StopName 1,200),(StopName 2, 50),(StopName 3,150),(StopName 4,100)] 10 (\_ -> 3) (StopName 1,60)
p1 = Params (\_ _ -> (5,10)) [(StopName 1,200),(StopName 2, 200)] 30 (\_ -> 3) (StopName 1,60)
    
main = simulate p0 >>= printLogs 1000 
