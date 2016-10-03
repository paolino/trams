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

data Min a = Min a deriving (Ord,Eq)
instance (Bounded a, Num a, Ord a) => Monoid (Min a) where
    mempty = Min maxBound
    Min x `mappend` Min y = Min $ min x y

-- | name of a Stop
type Destination = String

-- | part of some people
type Count = Int

-- | partition into destinations of a population
type People = M.Map Destination Count

-- | comonadic store for people at a stop (miss live randomization ?)
data PeopleStore = PeopleStore {
    update :: Time -> Maybe Count -> ([Person], PeopleStore)
    }

type Person = (Time,Destination)

compress :: [Destination] -> People
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
    
type Tram = People 

-- | Notion of time
type Time = Int

-- | type of events
data Event = Arrival Time Tram | Departure Time Tram deriving Show

instance F.Measured (Min Time) Event where
    measure (Arrival t _) = Min t
    measure (Departure t _) = Min t

-- | sequence selectable by min time
type Earliers a = F.FingerTree (Min Time) a

-- | correct in place or delete the earlier of the sequence
nextEarlier :: F.Measured (Min Time) a => Earliers a -> (a , Maybe a -> Earliers a)
nextEarlier t = let 
    (bs,F.viewl -> x F.:< cs) = F.split (== F.measure t) t
    in (x, (bs <>) . ($ cs) .  maybe id (F.<|))

-- | Stop state
data Stop = Stop {
    name :: Destination, -- ^ stop name
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

data Waiting = Waiting
    Destination
    Time -- ^ arrival of tram
    [Time] -- ^ people waiting times

    deriving Show
type MovePeople = Destination -> Time -> PeopleStore -> People -> Writer [Waiting] ((PeopleStore, People), Time)

--    update :: Time -> (People, People -> PeopleStore)

data Controls = Controls {
    speed :: Count -> Time,
    capacity :: Count
    }

movePeople :: Controls -> MovePeople
movePeople (Controls f cap) d t (PeopleStore upd)  p = let
    p' = M.delete d p -- dematerializing people
    n = M.findWithDefault 0 d p -- exiting tram count
    (unzip -> (ts,ds), ps') = upd t (Just $ cap - n) -- get most possible people from the stop
    dt = f $ max n $ length ts -- time for moving people
    in do
            tell [Waiting d t (map (flip subtract t) ts)]

            return ((ps',p' <> compress ds),dt)
    
    
setArrival :: Tram -> Time -> Stop -> Stop 
setArrival tr ti = over events $ (F.<|) (Arrival ti tr)

consumeEvent :: Controls -> Stop -> Writer [Waiting] (Stop,Maybe (Stop -> Stop))
consumeEvent cs s = case nextEarlier (view events s) of
    (Arrival ti tr,rf) -> do
        ((sp,tp),w) <- movePeople cs (name s) ti (view waiting s) tr
        let goodw = max w $ case F.viewl (view events s) of
                    (F.measure -> Min t) F.:< _ -> t 
        return (set waiting sp . set events (rf . Just $ Departure goodw tp) $ s, Nothing)
    (Departure ti tr, rf) -> return (set events (rf Nothing) s, Just $ setArrival tr (ti + distance s))

type Simulation = F.FingerTree (Min Time) Stop -- ^ circle of stops searcheable for events (is maybe events a monotonic monoid ?)

augmentedNextEarlier :: F.Measured (Min Time) a => Earliers a -> ((a,a), (a,a) -> Earliers a)
augmentedNextEarlier t = let 
    (bs,F.viewl -> x F.:< cs) = F.split (== F.measure t) t
    in case F.viewl cs of
        y F.:< cs' -> ((x,y), \(x,y) -> bs <> (x F.<| y F.<| cs'))
        _ -> case F.viewl bs of
            y F.:< bs' -> ((x,y), \(x,y) -> (y F.<| bs') <> (x F.<| cs))
            _ -> error "at least 2 stops for this price"

step :: Controls -> Simulation -> Writer [Waiting] Simulation
step cs s = let 
    ((x,y), corr) = augmentedNextEarlier s 
    in corr <$> second (maybe y ($ y)) <$> consumeEvent cs x 
    

data StopConfig = StopConfig 
    Destination -- stop name
    [Person] -- an infinite stream of person boarding
    Time -- distance int seconds to the next stop
    Int -- queue of departures trams, care! blocking the lane 
    Time -- departure frequency of the queue

mkStop :: StopConfig -> Stop
mkStop (StopConfig name ps d n dt) = Stop name d (mkPeopleStore ps) $ F.fromList $ unfoldr f (n,1) where
    f (0,t) = Nothing
    f (n,t) = Just (Departure t mempty,(n - 1,t + dt))
    
program     :: Controls  -- slowing function for boarding/landing + capacity
            -> [StopConfig] -- definition of stop
            -> [Waiting] -- a stream of simulation states
program cs scs = execWriter $ let f s = step cs s >>= f in f . F.fromList . map mkStop $ scs

-- example 
ranSim = do
    ns <- randomRIO (5,10)
    let names = map (("stop-" ++) . show ) [1..ns]
    ss <- forM names $ \name -> do
            ps <- do
                ts <- snd <$> mapAccumL (\a t -> (t + a,t + a)) 0 <$> randomRs (1,100) <$> newStdGen
                ns <- map (names !!) <$> randomRs (0,ns - 1) <$> newStdGen
                return $ zip ts ns
            d <- randomRIO (100,1000)
            n <- randomRIO (0,1)
            t <- randomRIO (20,30)
            return $ StopConfig name ps d n t
    friction <- (*) <$> randomRIO (1,3)
    cap <- randomRIO (50,300)
    return $ (Controls friction cap,ss)

main = uncurry program <$> ranSim >>= mapM_ print
