-- 8.1
data Caixa a = Um a | Dois a a | Tres a a a | Zero deriving (Show, Eq)
instance Functor Caixa where
    fmap _ Zero = Zero
    fmap g (Um x) = Um (g x)
    fmap g (Dois x y) = Dois (g x) (g y)
    fmap g (Tres x y z) = Tres (g x) (g y) (g z)

instance Applicative Caixa where
    pure = Um
    Zero <*> _ = Zero
    _ <*> Zero = Zero
    Um f <*> Um x = Um (f x)
    Um f <*> Dois x y = Dois (f x) (f y)
    Dois f g <*> Um x = Dois (f x) (g x)
    Dois f g <*> Dois x y = Dois (f x) (g y)
    Tres f g h <*> Um x = Tres (f x) (g x) (h x)
    Tres f g h <*> Dois x y = Tres (f x) (g y) (h y)
    Tres f g h <*> Tres x y z = Tres (f x) (g y) (h z)

instance Monad Caixa where
    return = pure
    Zero >>= _ = Zero
    (>>=) (Um x) f = f x
    (>>=) (Dois x y) f = f y
    (>>=) (Tres x y z) f = f z

-- 8.2

mult234 :: Double -> Caixa Double
mult234 x = return x >>= \ y -> Tres (2*y) (3*y) (4*y)

-- 8.3
-- a) Tres 1 2 3 >>= mult234 >>= mult234 = Tres 24.0 36.0 48.0
-- b) Dois 2 4 >>= mult234 = Tres 8.0 12.0 16.0
-- c) :kind Coisa = Coisa :: * -> *
-- d) Dois 2 3 >>= \_ -> Dois 7 7 = Dois 7 7

-- 8.4

func :: Caixa Double -> Caixa Double
func x = do 
        var <- Um 3
        var2 <- mult234 var 
        return var2
