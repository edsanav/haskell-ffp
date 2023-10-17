module ReaderPractice where
import Control.Applicative
import Data.Maybe

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

-- Remember functions are applicatives
bolt:: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x') = x'
fromMaybe x' Nothing = x'

