module CaseExample where

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"
  
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"
  
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs
  
greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "eyyyyy. What's shakin'?"
    False -> putStrLn "pshhhh."
  where cool = coolness == "downright frosty yo"