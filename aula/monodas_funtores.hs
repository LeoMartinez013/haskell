import Control.Monad

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
    mempty :: Num a => Sum a
    mempty   = Sum 0
    mappend :: Num a => Sum a -> Sum a -> Sum a
    mappend  = (<>)

sums :: [Sum Integer]
sums = [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]

-- Monoid concat
mconcat' :: (Monoid a) => [a] -> a
mconcat' xs = foldl (<>) mempty xs

-- buscaProd :: (Produto -> a) -> [Produto] -> ([a] -> a) -> a
-- buscaProd f ps g = g (map f ps)

-- -----------------------------------------------------

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

data Talvez a = Apenas a | Nada deriving Show

instance Functor Talvez where
    fmap func (Apenas a)    = Apenas (func a)
    fmap func Nada          = Nada

(//) :: (Eq a, Num a, Fractional a) => a -> a -> Talvez a
(//) x 0 = Nada
(//) x y = Apenas (x / y)

-- class Functor app => Applicative app where
--     pure :: a -> app a 
--     <*> :: app (a -> b) -> app a -> app b

instance Applicative Talvez where
    pure x = Apenas x
    -- (<*>) :: Talvez (a -> b) -> Talvez a -> Talvez b
    (<*>) (Apenas func) (Apenas a) = Apenas (func a)

{-
class Monad a where 
    return a :: m a
    join :: (Monad a) => m m a -> m a
-}

instance Monad Talvez where
    return a = Apenas a
    -- join (Apenas (Apenas a)) 
    (>>=) (Apenas x) func = func x

--  (>>=)   :: m a -> (a -> m b) -> m b
--  |>      :: a -> (a -> b) -> b -> a
--  ($)     :: (a -> b) -> a -> b
--  fmap    ::(a -> b) -> f a -> f b

main :: IO ()
main = 
    putStrLn "Digite o seu nome: " >>= \ x ->
    getLine >>= \ nome ->
    putStrLn ("Seja bem vindo: " ++ nome)
{-
    putStrLn "Digite o seu nome: "
    >>=
        \ _
        -> getLine
            >>= \ nome -> putStrLn "Seja bem vindo: " ++ nome return ()
-}
main' :: IO () -- Int
main' = do
    putStrLn "Digite um número: "
    y <- readLn :: IO Int
    let r = y + 2
    putStrLn $ show r -- Return r
