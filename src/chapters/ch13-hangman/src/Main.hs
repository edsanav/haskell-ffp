module Main (main) where

import Control.Monad (forever) -- [1]
import Data.Char (toLower) -- [2]
-- [3]
import Data.List (intersperse) -- [4]
import Data.Maybe (isJust)
import System.Exit (exitSuccess) -- [5]
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout) -- [6]
import System.Random (randomRIO) -- [7]

-- type WordList = [String]
newtype WordList = WordList [String] deriving (Eq, Show)


data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed strikes) =
    ( intersperse ' ' $
        fmap renderPuzzleChar discovered
    )
      ++ " Guessed so far: "
      ++ guessed
      ++ " \nRemaining lives: "
      ++ show (maxStrikes - strikes)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxStrikes::Int
maxStrikes = 7

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, l)
  return $ wl !! randomIndex
  where
    l = (length wl) -1

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (map (const Nothing) w) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ _) c = elem (toLower c) w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = elem c g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s strikes) c = (Puzzle word newFilledInSoFar (c : s) newStrikes)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar
    newStrikes =
      if filledInSoFar == newFilledInSoFar
        then strikes + 1
        else strikes

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case ( charInWord puzzle guess,
         alreadyGuessed puzzle guess
       ) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the\
        \ word, filling in the word\
        \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
        \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed strikes) =
  if strikes == maxStrikes then
    do putStrLn "You lose!"
       putStrLn $ "The word was "++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle (toLower c) >>= runGame
      _ -> putStrLn "Your guess must\
            \ be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = 
        freshPuzzle (fmap toLower word)
  runGame puzzle
