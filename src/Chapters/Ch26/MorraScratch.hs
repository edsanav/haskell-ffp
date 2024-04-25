module Chapters.Ch26.MorraScratch where
  
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad (unless)
  
type IOState s = StateT s IO 

data Choice = Odds | Evens deriving (Show, Eq)

data Player = Player String Choice deriving (Show, Eq)

data Play = Play Player Int deriving (Show, Eq)

--data Counter = 

play:: Play -> Play -> Player
play (Play p1@(Player _ c1) x) (Play p2@(Player _ _) y) = winner
  where evenResult = even (x + y) 
        winner = if (evenResult && c1 == Evens) || (not evenResult && c1 == Odds) 
                  then p1 
                  else p2


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