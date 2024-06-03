{-# LANGUAGE RecordWildCards #-}

module Scratch where

newtype Blah = Blah { myThing :: Int } deriving Show
data C = C {a :: Int, b :: Int, c :: Int, d :: Int}


wew :: Blah -> IO ()
wew Blah{..} = print myThing

wew2::C -> IO ()
wew2 C{..} = print c
