
module Timing ( getTick
              , timeIt
              ) where

import Control.Exception
import Data.Time.Clock
import Control.Monad.IO.Class
import System.IO.Unsafe

-- Timing functions

-- TODO: Consider just using the criterion package for all performance measurements
--       http://hackage.haskell.org/package/criterion

{-# NOINLINE startTime #-}
startTime :: UTCTime
startTime = unsafePerformIO getCurrentTime

-- In seconds
getTick :: IO Double
getTick = do
    -- Make sure startTime has been evaluated, otherwise the getCurrentTime in the
    -- unsafePerformIO might be evaluated after the getCurrentTime here, returning a
    -- negative tick on the first call to getTick
    st <- evaluate startTime
    (realToFrac . flip diffUTCTime st) <$> getCurrentTime

timeIt :: MonadIO m => m a -> m (Double, a)
timeIt f = do
    start <- liftIO getCurrentTime
    r <- f
    end <- liftIO getCurrentTime
    return (realToFrac $ diffUTCTime end start, r)

