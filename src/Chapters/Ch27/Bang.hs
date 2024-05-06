{-# LANGUAGE BangPatterns #-}

module Chapters.Ch27.Bang where

x = undefined
y = "bla"

main = do
  print (snd (x, x `seq` y))