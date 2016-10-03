
{-# language ViewPatterns#-}

module FingerTree where


import Data.FingerTree 
import Data.Monoid


data Min a = Min a deriving (Ord,Eq)
instance (Bounded a, Num a, Ord a) => Monoid (Min a) where
    mempty = Min maxBound
    Min x `mappend` Min y = Min $ min x y

-- | correct in place or delete the monoid culprit of the sequence, unsafe for mempty
selectByMeasure :: (Measured v a, Eq v) => FingerTree v a -> (a, Maybe a -> FingerTree v a)
selectByMeasure t = let 
    (bs,viewl -> x :< cs) = split (== measure t) t
    in (x, (bs <>) . ($ cs) .  maybe id (<|))

-- correcting 2 successive elements of a sequence turning around if necessary, fail if thera are less than 2 elems
biSelectByMeasure :: (Measured v a, Eq v) => FingerTree v a -> ((a, a), (a, a) -> FingerTree v a)
biSelectByMeasure t = let 
    -- find the culprit
    (bs,viewl -> x :< cs) = split (== measure t) t
    in case viewl cs of
        -- there is a successor 
        y :< cs' -> ((x,y), \(x,y) -> bs <> (x <| y <| cs'))
        -- there isn't
        _ -> case viewl bs of
            -- consider the head as the second element
            y :< bs' -> ((x,y), \(x,y) -> (y <| bs') <> (x <| cs))
            _ -> error "less than 2 elements"

swapHead (viewl -> (x :< (viewl -> y :< rs))) = y <| x <| rs
