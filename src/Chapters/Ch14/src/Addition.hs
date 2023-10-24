module Addition (sayHello, mainS) where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

myMult :: (Integral a) => a -> a -> a
myMult x y = case sameSign of
  True -> go x y (abs y)
  False -> negate $ go x y (abs y)
  where
    go a b count
      | count == 0 = 0
      | otherwise = (abs a) + (go a b (count -1))
    sameSign = (x >= 1 && y >= 1) || (x <= (-1) && y <= (-1))

mainS :: IO ()
mainS = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it
      "x + 1 is always\
      \ greater than x"
      $ do
        property $ \x -> x + 1 > (x :: Int)

mainDiv :: IO ()
mainDiv = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it
      "22 divided by 5 is \
      \4 remainder 2"
      $ do
        dividedBy 22 5 `shouldBe` (4, 2)

mainMult :: IO ()
mainMult = hspec $ do
  describe "Multiplication" $ do
    it "5 multiplied by 3 is 15" $ do
      myMult 5 3 `shouldBe` (15)
    it "5 multiplied by -3 is -15" $ do
      myMult 5 (-3) `shouldBe` (-15)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- i.e sample' (genTuple::Gen (Int, Float)) or...
--Prelude> type G = Gen (Int, Float)
--Prelude> sample (genTuple :: G)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency
    [ (1, return Nothing),
      (3, return (Just a))
    ]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater