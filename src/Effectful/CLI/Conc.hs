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

-- | @since 0.1.0.0
runConcurrently :: Concurrent :> es => Traversable t => t (Eff es a) -> Eff es (t a)
runConcurrently = mapConcurrently id

-- | @since 0.1.0.0
runConcurrently_ :: Concurrent :> es => Foldable t => t (Eff es a) -> Eff es ()
runConcurrently_ = mapConcurrently_ id

-- | @since 0.1.0.0
batchConcurrently :: Concurrent :> es => Int -> (a -> Eff es b) -> [a] -> Eff es [b]
batchConcurrently = pooledMapConcurrentlyN

-- | @since 0.1.0.0
batchConcurrently_ :: Concurrent :> es => Int -> (a -> Eff es b) -> [a] -> Eff es ()
batchConcurrently_ = pooledMapConcurrentlyN_

-- | @since 0.1.0.0
batchConcurrentlyWithIndex
  :: Concurrent :> es => Int -> (Int -> a -> Eff es b) -> [a] -> Eff es [b]
batchConcurrentlyWithIndex n f = batchConcurrently n (uncurry f) . zip [1 ..]

-- | @since 0.1.0.0
batchConcurrentlyWithIndex_
  :: Concurrent :> es => Int -> (Int -> a -> Eff es b) -> [a] -> Eff es ()
batchConcurrentlyWithIndex_ n f = batchConcurrently_ n (uncurry f) . zip [1 ..]
