module Chapters.Ch26.Exercises where

import Control.Monad.Trans.Reader
import Control.Monad.Identity

rDec::Num a => Reader a a
rDec = ReaderT $ Identity . (-) 1
--rDec = ReaderT (\r -> Identity (r - 1))


rShow::Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show
--rShow = ReaderT (\x -> Identity (show x))

rPrintAndInc::(Num a, Show a) => ReaderT a IO a
rPrintAndInc = undefined
