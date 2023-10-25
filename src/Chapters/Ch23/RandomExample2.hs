module Chapters.Ch23.RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

import Chapters.Ch23.RandomExample

rollDie:: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollDieThreeTimes'::State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- being an
infiniteDie::State StdGen [Die]
infiniteDie = repeat <$> rollDie

--Prelude> gen = (mkStdGen 0)
--Prelude> take 6 $ evalState infiniteDie gen
--[DieSix,DieSix,DieSix,DieSix,DieSix,DieSix]

-- That is because is we repeat a single die value, instead of the state
-- action that produces a die

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- *Chapters.Ch23.RandomExample2> evalState (nDie 5) (mkStdGen 0)
--[DieSix,DieSix,DieFour,DieOne,DieFive]


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go totalSum count gen
      | totalSum >= 20 = count
      | otherwise =
          let (die, nextGen) =
               randomR (1, 6) gen
          in go (totalSum + die)
                (count + 1) nextGen

-- instead of using (mkStdGen 0) with a 0, we can use randomIO to get a value
-- instead of 0 every time
--Prelude> :t randomIO
--randomIO :: Random a => IO a
--Prelude> rs = (rollsToGetTwenty . mkStdGen)

--Prelude> rs <$> randomIO
--6
--Prelude> rs <$> randomIO
--7

rollsToGetN::Int -> StdGen -> Int
rollsToGetN n = go 0 0 
  where
    go :: Int -> Int -> StdGen -> Int
    go totalSum count gen
      | totalSum >= n = count
      | otherwise =
          let (die, nextGen) =
               randomR (1, 6) gen
          in go (totalSum + die)
                (count + 1) nextGen
--  rs = (\n -> (rollsToGetN n) . mkStdGen )
--  (rs 100000 <$> randomIO)
-- 28476

rollsCountLogged::Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, []) 
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go totalSum (count, dies) gen
      | totalSum >= n = (count, reverse dies)
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (totalSum + die) (count + 1, intToDie die:dies) nextGen