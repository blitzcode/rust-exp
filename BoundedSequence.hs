
module BoundedSequence ( BoundedSequence
                       , empty
                       , push
                       , push_
                       , pop
                       , toList
                       , clear
                       ) where

import qualified Data.Sequence as S
import qualified Data.Foldable

-- Sequence with a stack interface which drops elements pushed over a specified depth

data BoundedSequence a = BoundedSequence !(S.Seq a) !Int
                         deriving (Show)

empty :: Int -> BoundedSequence a
empty limit | limit >= 1 = BoundedSequence S.empty limit
            | otherwise = error "limit for BoundedSequence needs to be >= 1"

-- Push element on the stack, truncate at the other end if we reached the limit,
-- return new stack and truncated element (if over the limit)
push :: a -> BoundedSequence a -> (BoundedSequence a, Maybe a)
push x (BoundedSequence s limit) =
    let seqDropR sd = case S.viewr sd of (s' S.:> e) -> (s', Just e)
                                         S.EmptyR -> (sd, Nothing)
        boundedS | S.length s >= limit = seqDropR s
                 | otherwise = (s, Nothing)
    in case boundedS of (s', e) -> (BoundedSequence (x S.<| s') limit, e)

push_ :: a -> BoundedSequence a -> BoundedSequence a
push_ x s = fst $ push x s

-- LIFO pop
pop :: BoundedSequence a -> (Maybe a, BoundedSequence a)
pop bs@(BoundedSequence s limit) =
    case S.viewl s of (x S.:< s') -> (Just x , BoundedSequence s' limit)
                      S.EmptyL -> (Nothing, bs)

toList :: BoundedSequence a -> [a]
toList (BoundedSequence s _) = Data.Foldable.toList s

clear :: BoundedSequence a -> BoundedSequence a
clear (BoundedSequence _ limit) = BoundedSequence S.empty limit

