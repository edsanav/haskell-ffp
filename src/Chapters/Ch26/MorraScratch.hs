module Chapters.Ch26.MorraScratch where
  
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad (unless)
  
type IOState s = StateT s IO 

data Player = OddsPlayer String | EvensPlayer String deriving (Show, Eq)

type Victories = Int
type OddsNumber = Int
type EvensNumber = Int

data Counter = Counter {getOddsP::(Player, Victories),getEvensP::(Player, Victories)} deriving (Show, Eq)

playOld:: (Player, Int) -> (Player, Int) -> Player
playOld (p1@(OddsPlayer _), x) (p2@(EvensPlayer _), y) = if odd (x + y) then p1 else p2
playOld (p1@(EvensPlayer _), x) (p2@(OddsPlayer _), y) = if even (x + y) then p1 else p2
playOld _ _ = error "Invalid play"

play::OddsNumber -> EvensNumber -> Counter -> (Counter, Player)
play x y (Counter (oddsP, oddsV) (evensP, evensV)) = 
  if odd (x + y) 
  then 
    let finalC = Counter (oddsP, oddsV + 1) (evensP, evensV)
    in (finalC, oddsP) 
  else
    let finalC = Counter(oddsP, oddsV) (evensP, evensV+1)
    in (finalC, evensP)

game :: IOState Counter Player
game = do 
   c <- get
   _ <- lift $ putStr "P:"
   oddsPlay <- lift $ fmap (read::String -> Int) getLine
   _ <- lift $ putStr "P:"
   evensPlay <- lift $ fmap (read::String -> Int) getLine
   _ <- put $ play oddsPlay evensPlay c 
   return odds

loop :: IOState Counter ()
loop = do 
  _ <- lift $ putStrLn "Choose a number between 0 and 5"
  myNumber <- lift $ fmap (read::String -> Int) getLine
  unless (myNumber > 5) loop


startGame1::IO Counter
startGame1 = do
  _ <- putStrLn "-- P is Player"
  _ <- putStrLn "-- C is Computer"
  _ <- putStrLn "-- Playser is odds, Computer is evens"
  return $ Counter (OddsPlayer "P",  0) (EvensPlayer "C", 0)

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
main = do
  initCounter <- startGame1
  fst <$> runStateT loop initCounter