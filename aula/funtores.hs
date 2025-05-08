
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