module OptMonoid where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada Nada = Nada
    (<>) (Only x) (Only y) = Only (x <> y)
    (<>) x@(Only _) _ = x
    (<>) _ x@(Only _) = x

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
