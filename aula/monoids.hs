-- Monoids

data Sum a = Sum {getSum :: a} deriving Show
data Mult a = Mult {getMult :: a} deriving Show

-- instance (Monoid a) => Monoid Mult (Sum a) where
{-
CONTEUDO QUE NÃO ESTÁ NO LIVRO: 
-}
instance (Num a) => Semigroup (Sum a) where
    (<>) (Sum x) (Sum y) = Sum (x + y)

instance (Num a) => Monoid (Sum a) where
    mempty   = Sum 0
    mappend :: Num a => Sum a -> Sum a -> Sum a
    mappend  = (<>)

sums :: [Sum Integer]
sums = [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]

-- Monoid concat
mconcat' :: (Monoid a) => [a] -> a
mconcat' xs = foldl (<>) mempty xs
