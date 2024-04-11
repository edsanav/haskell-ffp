module Chapters.Ch26.FixMe where

import Control.Monad.Trans.Maybe
import Control.Monad
  
isValid::String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- MaybeT $ fmap Just getLine
  guard $ isValid v
  return v


doExcite :: IO()
doExcite = do 
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite

  case excite of 
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn ("Good, was very excite: " ++ e)