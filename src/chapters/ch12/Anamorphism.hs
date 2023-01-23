module Anamorphism where

myIterate :: (a->a) -> a -> [a]
myIterate f seed = seed:myIterate f (f seed)

myUnfoldr::(b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f seed = case f seed of
  (Just (current, nextSeed)) -> current:(myUnfoldr f nextSeed)
  Nothing -> []
  
betterIterate ::  (a->a) -> a -> [a]
betterIterate f seed = myUnfoldr (\b -> Just(b, f b)) seed