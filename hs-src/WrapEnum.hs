
module WrapEnum where

-- Move through an enumeration, but wrap around when hitting the end
wrapSucc, wrapPred :: (Enum a, Bounded a, Eq a) => a -> a
wrapSucc a | a == maxBound = minBound
           | otherwise     = succ a
wrapPred a | a == minBound = maxBound
           | otherwise     = pred a

