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
import System.Random (newStdGen,randomRs,randomRIO,Random)

-- local
import FingerTree (biSelectByMeasure, selectByMeasure, Min (..))

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
    _events :: FingerTree (Min Time) Event -- ^ possible next event
    }
makeLenses ''Stop

instance Measured (Min Time) Stop where
    measure = measure . view events
--------------------------------------------------------------------------------
--------- cosuming events ------------------------------------------------------
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
            let h@(Event tif htr) :< _ = viewl es
            case tid htr == tid tr of
                -- if it's not the first in the queue , Just update the scheduling to the departure time of the first (+ something ?)
                False -> do
                    tell [Queued (tid tr) (name s) ti]
                    return (set events (rf . Just $ Event tif tr) $ s, Nothing)
                -- if it's the first check departing conditions
                True -> do
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
    [TramId] -- queue of departures trams, care! blocking the lane 
    Time -- departure frequency of the queue

-- live stop
mkStop :: StopConfig -> Stop
mkStop (StopConfig name ps d tis dt) = Stop name d (mkPeopleStore ps) $ fromList $ unfoldr f (tis,1) where
    f ([],t) = Nothing
    f (n:ns,t) = Just (Event t (Tram n mempty),(ns,t + dt))

-- produce an infinite log from the simulation
program     :: Controls  
            -> [StopConfig] 
            -> [Log Time] -- a stream of simulation states
program cs = execWriter . let f s = step cs s >>= f in f . fromList . map mkStop 


-- all times in seconds, variances
data Params = Params {
    peopleArrivalFreq :: (Time,Time), --
    stopDistances :: (Time,Time),
    departingInitiaDelays :: (Time,Time),
    capacitiesOfTrams :: (Count,Count),
    boardingPersonTimes :: (Time,Time),
    numberOfStops :: (Int,Int),
    numberOfTrams :: (Int,Int)
    }



ranSim (Params paf sd did cot bpt nos not) = do
    -- number of stops
    ns <- randomRIO nos
    -- stop names
    let names = map StopName [1..ns]
    -- number of trams
    ntis <- randomRIO not
    -- trams id and their starting stop
    tsts <- forM [1..ntis] $ \ti -> (,) (TramId ti) <$> (names !!) <$> randomRIO (0,ns - 1)

    ss <- forM names $ \name -> do
            ps <- do
                
                ts <- snd <$> mapAccumL (\a t -> (t + a,t + a)) 0 <$> randomRs paf <$> newStdGen
                ns <- filter (/= name) <$> map (names !!) <$> randomRs (0,ns - 1) <$> newStdGen
                return $ zip ts ns
            d <- randomRIO sd
            let tis = map fst . filter ((==) name . snd) $ tsts
            t <- randomRIO did
            return $ StopConfig name ps d tis t
    friction <- (\t (Count c) -> t * c) <$> randomRIO bpt
    cap <- randomRIO cot
    return $ (Controls friction cap, ss)

data Minutes = Minutes Int Int

instance Show Minutes where
    show (Minutes n s) = show n ++ "'" ++ show s ++"\""

fromSeconds = uncurry Minutes . (`divMod` 60)

toMinutes (Operating tis sn t cd ts) = Operating tis sn (fromSeconds t) cd (map fromSeconds ts) 
toMinutes (Queued tis sn t) = Queued tis sn $ fromSeconds t
toMinutes (Leaving tis sn t) = Queued tis sn $ fromSeconds t
-- example 
p0 = Params (1,100) (60,300) (30,200) (100,200) (1,3) (5,15) (1,4)

simulate p = uncurry program <$> ranSim p 
printLogs n = mapM_ (print . toMinutes) . take n 
