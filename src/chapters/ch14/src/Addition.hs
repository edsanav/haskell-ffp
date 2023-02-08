module Addition (sayHello, mainS) where

import Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

myMult::(Integral a) => a -> a -> a
myMult x y = case sameSign of
  True -> go x y (abs y)
  False -> negate $ go x y (abs y)
  where go a b count
          | count == 0 = 0
          | otherwise = (abs a) + (go a b (count-1))
        sameSign = (x >= 1 && y >= 1) || (x <= (-1) && y<=(-1))

mainS :: IO ()
mainS = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4

mainDiv :: IO ()
mainDiv = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is \
        \4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

mainMult :: IO ()
mainMult = hspec $ do
  describe "Multiplication" $ do
    it "5 multiplied by 3 is 15" $ do
      myMult 5 3 `shouldBe` (15)
    it "5 multiplied by -3 is -15" $ do
      myMult 5 (-3) `shouldBe` (-15)

