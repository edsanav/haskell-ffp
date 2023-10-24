module OptMonoid where
  
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity::(Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity::(Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada Nada = Nada
    (<>) (Only x) (Only y) = Only (x <> y)
    (<>) x@(Only _) _ = x
    (<>) _ x@(Only _) = x

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

newtype First' a = 
  First' { getFirst' :: Optional a}
  deriving (Eq, Show)
  
instance Semigroup (First' a) where
  (<>) fi@(First' (Only _)) _ = fi
  (<>) (First' Nada) fi@(First' (Only _)) = fi
  (<>) _ _ = First' Nada
  
instance Monoid (First' a) where
  mempty = First' Nada
  
instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    elements [First' Nada, First' (Only a)]
    
firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

check::IO()
check = do 
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)