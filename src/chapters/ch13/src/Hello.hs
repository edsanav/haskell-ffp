module Hello (sayHello) where

sayHello :: IO ()
sayHello = do
  putStr "hello world"

