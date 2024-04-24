module Chapters.Ch26.MorraScratch where
  
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad (unless)
  
type IOState s = StateT s IO 

loop :: IOState () ()
loop = do 
  _ <- lift $ putStrLn "Choose a number between 0 and 5"
  myNumber <- lift $ fmap (read::String -> Int) getLine
  unless (myNumber > 5) loop

  
loopExample :: IO ()
loopExample = do 
  inp <- getLine
  case inp of 
    "END" -> putStrLn "Finishing..."
    x -> do
      _ <- putStrLn ("Received "++x++ " as input")
      _ <- putStrLn "New loop iteration"
      loopExample
  
main :: IO ()
main = fst <$> runStateT loop ()