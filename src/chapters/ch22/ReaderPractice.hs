module ReaderPractice where
import Control.Applicative
import Data.Maybe
import Data.Foldable (fold)
import Data.Monoid

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]


xs::Maybe Integer
xs = lookup 3 $ zip x y

ys:: Maybe Integer
ys = lookup 6 $ zip y z

zs:: Maybe Integer
zs = lookup 4 $ zip x y

z':: Integer -> Maybe Integer
z' = flip lookup (zip x z)

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- TODO probably there's a nicer way
x3 :: Integer -> Maybe (Integer, Integer)
x3 n = (,) <$> z' n <*> z' n


summed :: Num c => (c, c) -> c
summed = uncurry (+)

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

-- Remember functions are applicatives
bolt:: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' _ (Just x') = x'
fromMaybe' x' Nothing = x'

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]



main:: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  -- the function is the applicative -> [(a->b)] to a->[b]
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldMap All (sequA 7) -- equivalent to fold $ fmap All (sequA 7)
  print $ traverse sequA s'
  print $ sequA $ fromMaybe 8 s'
  -- bolt -- Intrgrt -> Bool
  -- xs -- Maybe Integer
  print $ (bolt.fromMaybe 8) xs
  print $ fmap bolt (fromMaybe 8) xs
