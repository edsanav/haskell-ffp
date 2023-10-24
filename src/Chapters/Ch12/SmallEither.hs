

selectLeft::Either a b -> [a] -> [a]
selectLeft (Left x) acc = (x:acc)
selectLeft _ acc = acc

selectRight::Either a b -> [b] -> [b]
selectRight (Right x) acc = (x:acc)
selectRight _ acc = acc

selectBoth::Either a b -> ([a],[b]) -> ([a],[b])
selectBoth (Left x) (lAcc, rAcc) = (x:lAcc, rAcc)
selectBoth (Right x) (lAcc, rAcc) = (lAcc, x:rAcc)


lefts'::[Either a b] -> [a]
lefts' xs = foldr selectLeft [] xs

rights'::[Either a b] -> [a]
rights' xs = foldr selectLeft [] xs

partitionEithers'::[Either a b] -> ([a],[b])
partitionEithers' xs = foldr selectBoth ([],[]) xs

either'::(a->c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a 
either' _ f (Right b) = f b 

eitherMaybe''::(b->c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\x -> Just (f x))