module Chapters.Ch26.Exercises where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Control.Monad.Trans.State

rDec::Num a => Reader a a
rDec = ReaderT $ Identity . (-) 1
--rDec = ReaderT (\r -> Identity (r - 1))


rShow::Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show
--rShow = ReaderT (\x -> Identity (show x))

rPrintAndInc::(Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT (\r -> do 
                          print ("Hi: " ++ show r)
--                          liftIO $ putStrLn ("Hi: " ++ show r)
                          return (r+1)
                        )

sPrintIncAccum::(Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT (\a -> do
                          print ("Hi: " ++ show a)
                          return (show a, a + 1)
                          )
  