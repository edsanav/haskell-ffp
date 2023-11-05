module Chapters.Ch23.Exercises where

import Chapters.Ch23.ScratchInstance



get::Moi s s
get = Moi (\x -> (x,x))

put :: s -> Moi s ()
put s = Moi (const ((), s))
-- same as Moi (\_ -> ((), s))

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

eval:: Moi s a -> s -> a
eval (Moi sa) = fst.sa

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))

-- ghci> runMoi f 0
-- ((),1)
-- ghci> runMoi (f >> f) 0
-- ((),2)