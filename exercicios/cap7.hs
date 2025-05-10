-- EX 7.1
data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving (Show, Eq)

instance Functor Coisa where
    fmap g ZeroCoisa = ZeroCoisa
    fmap g (UmaCoisa x) = UmaCoisa (g x)
    fmap g (DuasCoisas x y) = DuasCoisas (g x) (g y)

-- Ex 7.2
instance Applicative Coisa where
    pure = UmaCoisa
    ZeroCoisa <*> _ = ZeroCoisa
    _ <*> ZeroCoisa = ZeroCoisa
    UmaCoisa f <*> UmaCoisa x = UmaCoisa (f x)
    UmaCoisa f <*> DuasCoisas x y = DuasCoisas (f x) (f y)
    DuasCoisas f g <*> UmaCoisa x = DuasCoisas (f x) (g x)
    DuasCoisas f g <*> DuasCoisas x y = DuasCoisas (f x) (g y)

-- EX 7.3
-- mult234 :: Double -> Coisa Double
-- mult234 x = UmaCoisa (x * 2) <*> UmaCoisa (x * 3) <*> UmaCoisa (x * 4)
