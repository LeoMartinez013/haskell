-- EX 7.1
data Coisa a = UmaCoisa a | DuasCoisas a a | TresCoisas a a a | ZeroCoisa deriving (Show, Eq)

instance Functor Coisa where
    fmap g ZeroCoisa = ZeroCoisa
    fmap g (UmaCoisa x) = UmaCoisa (g x)
    fmap g (DuasCoisas x y) = DuasCoisas (g x) (g y)
    fmap g (TresCoisas x y z) = TresCoisas (g x) (g y) (g z)

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
mult234 :: Double -> Coisa Double
mult234 x = TresCoisas (x * 2) (x * 3) (x * 4)

-- EX 7.4
data Arvore a = Vazia | Folha a | Raiz a (Arvore a) (Arvore a) deriving (Show, Eq)

instance Functor Arvore where
    fmap _ Vazia = Vazia
    fmap f (Folha x) = Folha (f x)
    fmap f (Raiz x esq dir) = Raiz (f x) (fmap f esq) (fmap f dir)

instance Applicative Arvore where
    pure = Folha
    Vazia <*> _ = Vazia
    _ <*> Vazia = Vazia
    Folha f <*> Folha x = Folha (f x)
    Folha f <*> Raiz x esq dir = Raiz (f x) (fmap f esq) (fmap f dir)
    Raiz f1 e1 d1 <*> Raiz f2 e2 d2 = Raiz (f1 f2) (e1 <*> e2) (d1 <*> d2)

-- EX 7.5 
data Fantasma a = Fantasma deriving (Show , Eq)

instance Functor Fantasma where
    fmap _ Fantasma = Fantasma

