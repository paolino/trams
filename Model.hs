{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses#-}
{-# language FlexibleInstances#-}
{-# language FlexibleContexts#-}
{-# language ViewPatterns#-}
{-# language GeneralizedNewtypeDeriving#-}
{-# language DeriveFunctor#-}

module Model (
    -- * types
    Time,
    StopName (..) ,
    Count (..),
    Ctx (..),
    Log (..),
    Traveller,
    TramId (..),
    StopConfig (..),
    Controls (..),
    -- * function
    simulation 
    )
     

where

import Data.Map (findWithDefault, delete,insertWith , Map) 
import Data.FingerTree (ViewL((:<)),ViewR((:>)),(<|),(|>),viewl,FingerTree,Measured(..),split,fromList)
import Data.Monoid ((<>))
import Control.Monad (forM)
import Control.Monad.Writer (tell,execWriter,Writer)
import Control.Monad.Reader (runReaderT,ReaderT, ask)
import Control.Arrow (second)
import Data.List (mapAccumL)
import Control.Lens hiding ((:<),(:>),(<|),(|>),viewl)
import Control.Lens.TH (makeLenses, makePrisms)

-- local
import FingerTree (biSelectByMeasure, selectByMeasure, Min (..),swapHead)

-- | Notion of time 
type Time = Int

-- | name of a Stop
newtype StopName = StopName Int deriving (Eq,Show,Ord)

-- | count of people
newtype Count = Count Int deriving (Show, Num,Eq, Ord)

--  partition into destinations of a population
type People = Map StopName Count

--  comonadic store for people at a stop (miss live randomization ?)
data PeopleStore = PeopleStore {
    update :: Time -> Maybe Count -> ([Traveller], PeopleStore)
    }

-- | traveller
type Traveller = (Time,StopName)

compress :: [StopName] -> People
compress ds = foldr (uncurry $ insertWith (+)) mempty $ zip ds $ repeat 1

mkPeopleStore :: [Traveller] -> PeopleStore 
mkPeopleStore = make  where
    u rs t mc = let
        (us,rs') = break ((>t) . fst) rs
        (us',us'') = case mc of
                Just (Count m) -> splitAt m us
                Nothing -> (us,[])
        in (us', make $ us'' ++ rs' )
    make rs = PeopleStore (u rs) 

-- | tram plate
newtype TramId = TramId Int deriving (Show,Eq)

-- unsatisfactory partial tram vision
data Tram = Tram {
    tid :: TramId,
    _carrying :: People
    }
    | Fresh {
    tid :: TramId
    }
    deriving Show
makeLenses ''Tram


--  A scheduled departure time for a tram
data Event = Event Time Tram deriving Show

instance Measured (Min Time) Event where
    measure (Event t _) = Min t

--  sequence selectable by min time
type Earliers a = FingerTree (Min Time) a

--  Stop state
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
--------- Environment ----------------------------------------------------------
--------------------------------------------------------------------------------
-- | log message context
data Ctx t = Ctx 
    TramId -- ^ logging tram
    StopName -- ^ logging stop
    t -- ^ logging time
    deriving (Show, Functor)

-- | log message
data Log t 
    -- | tram has operated
    = Operating 
        (Ctx t) -- ^ logging context
        Count -- ^ number of unboarded people 
        [t] -- ^ boarded people waiting times
    -- | tram is waiting
    | Queued
        (Ctx t) -- ^ logging context
    -- | tram is leaving
    | Leaving 
        (Ctx t) -- ^ logging context
    deriving (Show, Functor)


data Controls = Controls {
    speed :: Count -> Time,
    capacity :: Count
    }

type Env = ReaderT Controls (Writer [Log Time])

--------------------------------------------------------------------------------
--------------Core logic--------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

movePeople :: Ctx Time -> PeopleStore -> People -> Env (Time, Maybe (PeopleStore, People)) 
movePeople  ctx@(Ctx _ d t) (PeopleStore upd)  p = do
    Controls dwell cap <- ask
    let     n = findWithDefault 0 d p -- exiting tram people count
            (unzip -> (ts,ds), ps') = upd t (Just $ cap - n) -- get most possible people from the stop
            m = max n . Count $ length ts -- max number of peple boarding vs landing
    (,) (dwell m) <$> case m of
        0 ->    do  
                    tell [Leaving ctx]
                    return Nothing
        m ->    do
                    tell [Operating ctx n (map (flip subtract t) ts)]
                    return $ Just  (ps',delete d p <> compress ds)
    
   
consumeEvent :: Stop -> Env (Stop,Maybe (Stop -> Stop))
consumeEvent s = let
    es = view events s
    in case selectByMeasure es of
        (e@(Event ti tr), rf) -> do
            -- get the first of the queue
            let     h@(Event tif htr) :< rs = viewl es
                    ctx = Ctx (tid tr) (name s) ti
            case tid htr == tid tr of
                -- if it's not the first in the queue , Just update the scheduling to the departure time of the first (+ something ?)
                False -> do
                    tell [Queued ctx]
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
                            (w , t) <- movePeople ctx (view waiting s) (view carrying tr)
                            case t of
                                -- if they are met leave the queue and transpose as arrival to the next stop
                                Nothing -> return (set events (rf Nothing) $ s, Just $ over events (|> Event (ti + w + distance s) tr))
                                -- if they are not met reschedule for loading more people
                                Just (sp,tp) -> return (set waiting sp . set events (rf . Just $ Event (w + ti) (set carrying tp tr)) $ s, Nothing )

--  circle of stops  
type Track = FingerTree (Min Time) Stop 

-- step the track, logging the boarding
step :: Track -> Env Track
step s = corr <$> second (maybe y ($ y)) <$> consumeEvent x where
    ((x,y), corr) = biSelectByMeasure s 
    
-- | configuration of a stop
data StopConfig = StopConfig 
    StopName -- ^ stop name
    [Traveller] -- ^ an infinite stream of person boarding
    Time -- ^ distance int seconds to the next stop
    (Maybe ([TramId], Time)) -- ^ queue of ids for departing trams at given freqs

-- live stop
mkStop :: StopConfig -> Stop
mkStop (StopConfig name ps d mh) = let 
        c = Stop name d (mkPeopleStore ps) 
        in case mh of
                Nothing -> c mempty [] (error "using a trams we don't have")
                Just (ti:tis,dt) -> c (fromList [Event 0 (Fresh ti)]) tis dt where

-- | produce an infinite log from the simulation
simulation     :: Controls  -- ^ simulation controls
            -> [StopConfig] -- ^ stops definition
            -> [Log Time] -- ^ a stream of simulation states
simulation cs = execWriter . flip runReaderT cs .  f . fromList . map mkStop  where
                f s = step s >>= f 

