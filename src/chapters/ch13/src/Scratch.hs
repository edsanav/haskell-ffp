module PartiallyApplied (twooPrint) where

twoo :: IO Bool
twoo = do
  c <- getChar
  c' <- getChar
  return $ c == c'


twooPrint :: IO ()
twooPrint = do
  c <- getChar
  c' <- getChar
  if c == c'
    then putStrLn "THEY ARE EQUAL"
    else return ()
