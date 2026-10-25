module Effectful.CLI.Conc (
  concurrently,
  concurrently_,
  runConcurrently,
  runConcurrently_,
  mapConcurrently,
  mapConcurrently_,
  forConcurrently,
  forConcurrently_,
  batchConcurrently,
  batchConcurrently_,
  batchConcurrentlyWithIndex,
  batchConcurrentlyWithIndex_,
) where

import Effectful (Eff, (:>))
import Effectful.Concurrent.Async (
  Concurrent,
  concurrently,
  concurrently_,
  forConcurrently,
  forConcurrently_,
  mapConcurrently,
  mapConcurrently_,
  pooledMapConcurrentlyN,
  pooledMapConcurrentlyN_,
 )

-- | Run all actions concurrently and collect their results.
--
-- @since 0.1.0.0
runConcurrently
  :: Concurrent :> es
  => Traversable t
  => t (Eff es a)
  -> Eff es (t a)
runConcurrently = mapConcurrently id

-- | Run all actions concurrently, discarding results.
--
-- @since 0.1.0.0
runConcurrently_
  :: Concurrent :> es
  => Foldable t
  => t (Eff es a)
  -> Eff es ()
runConcurrently_ = mapConcurrently_ id

-- | Map over a list with at most @n@ concurrent actions.
--
-- @since 0.1.0.0
batchConcurrently
  :: Concurrent :> es
  => Int
  -> (a -> Eff es b)
  -> [a]
  -> Eff es [b]
batchConcurrently = pooledMapConcurrentlyN

-- | Like 'batchConcurrently', discarding results.
--
-- @since 0.1.0.0
batchConcurrently_
  :: Concurrent :> es
  => Int
  -> (a -> Eff es b)
  -> [a]
  -> Eff es ()
batchConcurrently_ = pooledMapConcurrentlyN_

-- | Like 'batchConcurrently', also passing each element's 1-based index.
--
-- @since 0.1.0.0
batchConcurrentlyWithIndex
  :: Concurrent :> es
  => Int
  -> (Int -> a -> Eff es b)
  -> [a]
  -> Eff es [b]
batchConcurrentlyWithIndex n f = batchConcurrently n (uncurry f) . zip [1 ..]

-- | Like 'batchConcurrentlyWithIndex', discarding results.
--
-- @since 0.1.0.0
batchConcurrentlyWithIndex_
  :: Concurrent :> es
  => Int
  -> (Int -> a -> Eff es b)
  -> [a]
  -> Eff es ()
batchConcurrentlyWithIndex_ n f = batchConcurrently_ n (uncurry f) . zip [1 ..]
