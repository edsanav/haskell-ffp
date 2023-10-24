module GreetIfCool2 where
  
 
greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then putStrLn "eyyyy, What's shaking'?"
  else
    putStrLn "pshhhhh."
  where cool v = v == "dowright frosty yo"