module MyZipList where

import Test.QuickCheck.Checkers

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take 3000 l
      ys' =
        let (ZipList' l) = ys
         in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs
    
instance Applicative ZipList' where
  pure = ZipList' . repeat 
  (<*>) (ZipList' []) (ZipList' []) = ZipList' []
  (<*>) (ZipList' []) (ZipList' _) = ZipList' []
  (<*>) (ZipList' _)  (ZipList' []) = ZipList' []
  (<*>) (ZipList' xsf) (ZipList' xs) = ZipList' $ map (\(f, x) -> f x) (zip xsf xs)
--    Or alternatively
--    pure = ZipList' . repeat 
--    (<*>) (ZipList' xsf) (ZipList' xs) = ZipList' $ zipWith ($) xsf xs
                                              
  
