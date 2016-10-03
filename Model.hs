{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses#-}
{-# language FlexibleInstances#-}
{-# language FlexibleContexts#-}
{-# language ViewPatterns#-}

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.FingerTree as F
import Data.Monoid
import Control.Monad (forM)
import Control.Monad.Writer
import Control.Arrow (second)
import Data.List (unfoldr,mapAccumL)

import Control.Lens
import Control.Lens.TH
import Data.Foldable

import System.Random

-- | Notion of time
type Time = Int

data Min a = Min a deriving (Ord,Eq)
instance (Bounded a, Num a, Ord a) => Monoid (Min a) where
    mempty = Min maxBound
    Min x `mappend` Min y = Min $ min x y

-- | name of a Stop
newtype StopName = StopName Int deriving (Eq,Show,Ord)

-- | part of some people
type Count = Int

-- | partition into destinations of a population
type People = M.Map StopName Count

-- | comonadic store for people at a stop (miss live randomization ?)
data PeopleStore = PeopleStore {
    update :: Time -> Maybe Count -> ([Person], PeopleStore)
    }

type Person = (Time,StopName)

compress :: [StopName] -> People
compress ds = foldr (uncurry $ M.insertWith (+)) mempty $ zip ds $ repeat 1

mkPeopleStore :: [Person] -> PeopleStore 
mkPeopleStore = make  where
    u rs t mc = let
        (us,rs') = break ((>t) . fst) rs
        (us',us'') = case mc of
                Just m -> splitAt m us
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

instance F.Measured (Min Time) Event where
    measure (Event t _) = Min t

-- | sequence selectable by min time
type Earliers a = F.FingerTree (Min Time) a

-- | correct in place or delete the earlier of the sequence, unsafe
nextEarlier :: F.Measured (Min Time) a => Earliers a ->  (a , Maybe a -> Earliers a)
nextEarlier t = let 
    (bs,F.viewl -> x F.:< cs) = F.split (== F.measure t) t
    in (x, (bs <>) . ($ cs) .  maybe id (F.<|))

-- | Stop state
data Stop = Stop {
    name :: StopName, -- ^ stop name
    distance :: Time, -- ^ distance to the  next stop
    _waiting :: PeopleStore, -- ^ people at the stop
    _events :: F.FingerTree (Min Time) Event -- ^ possible next event
    }
makeLenses ''Stop

instance F.Measured (Min Time) Stop where
    measure = F.measure . view events
--------------------------------------------------------------------------------
--------- cosuming events ------------------------------------------------------
--------------------------------------------------------------------------------

data Log = Log
    TramId
    StopName
    Time -- ^ arrival of tram
    [Time] -- ^ people waiting times

    deriving Show

data Controls = Controls {
    speed :: Count -> Time,
    capacity :: Count
    }

movePeople :: Controls -> TramId -> StopName -> Time -> PeopleStore -> People -> Writer [Log] (Time, Maybe (PeopleStore, People)) 
movePeople (Controls f cap) tid d t (PeopleStore upd)  p = let
    p' = M.delete d p -- dematerializing people
    n = M.findWithDefault 0 d p -- exiting tram count
    (unzip -> (ts,ds), ps') = upd t (Just $ cap - n) -- get most possible people from the stop
    m = max n $ length ts
    w = f m
    in case m of
        0 -> return (w, Nothing)
        m -> do 
            tell [Log tid d t (map (flip subtract t) ts)]
            return (w,Just  (ps',p' <> compress ds))
    
    

consumeEvent :: Controls -> Stop -> Writer [Log] (Stop,Maybe (Stop -> Stop))
consumeEvent cs s = let
    es = view events s
    in case nextEarlier es of
        (e@(Event ti tr), rf) -> do
            let h@(Event tif htr) F.:< _ = F.viewl es
            case tid htr == tid tr of
                -- if it's not the first in the queue , Just update the scheduling to the departure time of the first (+ something ?)
                False -> return (set events (rf . Just $ Event tif tr) $ s, Nothing)
                -- if it's the first check departing conditions
                True -> do
                    (w , t) <- movePeople cs (tid tr) (name s) ti (view waiting s) (view carrying tr)
                    case t of
                        -- if they are met leave the queue and transpose as arrival to the next stop
                        Nothing -> return (set events (rf Nothing) $ s, Just $ over events (F.|> Event (ti + w + distance s) tr))
                        -- if they are not met reschedule for loading more people
                        Just (sp,tp) -> return (set waiting sp . set events (rf . Just $ Event (w + ti) (set carrying tp tr)) $ s, Nothing )


type Simulation = F.FingerTree (Min Time) Stop -- ^ circle of stops searcheable for events (is maybe events a monotonic monoid ?)

augmentedNextEarlier :: F.Measured (Min Time) a => Earliers a -> ((a,a), (a,a) -> Earliers a)
augmentedNextEarlier t = let 
    (bs,F.viewl -> x F.:< cs) = F.split (== F.measure t) t
    in case F.viewl cs of
        y F.:< cs' -> ((x,y), \(x,y) -> bs <> (x F.<| y F.<| cs'))
        _ -> case F.viewl bs of
            y F.:< bs' -> ((x,y), \(x,y) -> (y F.<| bs') <> (x F.<| cs))
            _ -> error "at least 2 stops for this price"

step :: Controls -> Simulation -> Writer [Log] Simulation
step cs s = let 
    ((x,y), corr) = augmentedNextEarlier s 
    in corr <$> second (maybe y ($ y)) <$> consumeEvent cs x 
    

data StopConfig = StopConfig 
    StopName -- stop name
    [Person] -- an infinite stream of person boarding
    Time -- distance int seconds to the next stop
    [TramId] -- queue of departures trams, care! blocking the lane 
    Time -- departure frequency of the queue

mkStop :: StopConfig -> Stop
mkStop (StopConfig name ps d tis dt) = Stop name d (mkPeopleStore ps) $ F.fromList $ unfoldr f (tis,1) where
    f ([],t) = Nothing
    f (n:ns,t) = Just (Event t (Tram n mempty),(ns,t + dt))
    
program     :: Controls  -- slowing function for boarding/landing + capacity
            -> [StopConfig] -- definition of stop
            -> [Log] -- a stream of simulation states
program cs scs = execWriter $ let f s = step cs s >>= f in f . F.fromList . map mkStop $ scs


-- example 

ranSim = do
    -- number of stops
    ns <- randomRIO (5,10)
    -- stop names
    let names = map StopName [1..ns]
    -- number of trams
    ntis <- randomRIO (1,ns)
    -- trams id and their starting stop
    tsts <- forM [1..ntis] $ \ti -> (,) (TramId ti) <$> (names !!) <$> randomRIO (0,ns - 1)

    ss <- forM names $ \name -> do
            ps <- do
                
                ts <- snd <$> mapAccumL (\a t -> (t + a,t + a)) 0 <$> randomRs (1,100) <$> newStdGen
                ns <- filter (/= name) <$> map (names !!) <$> randomRs (0,ns - 1) <$> newStdGen
                return $ zip ts ns
            d <- randomRIO (100,1000)
            let tis = map fst . filter ((==) name . snd) $ tsts
            t <- randomRIO (20,30)
            return $ StopConfig name ps d tis t
    friction <- (*) <$> randomRIO (1,3)
    cap <- randomRIO (50,300)
    return $ (Controls friction cap,ss)

toMinutes (Log tis sn t ts) = Log tis sn (t `div` 60) (map (`div` 60) ts)

nmain n = uncurry program <$> ranSim >>= mapM_ print . map toMinutes . take n
