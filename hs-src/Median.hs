
module Median ( median
              ) where

import Data.List

median :: (Fractional a, Ord a) => [a] -> Maybe a
median xs | null xs   = Nothing
          | odd  len  = Just $ sorted !! mid
          | even len  = Just meanMedian
          | otherwise = Nothing
  where len        = length sorted
        mid        = len `div` 2
        meanMedian = (sorted !! (mid - 1) + sorted !! mid) / 2
        sorted     = sort xs

