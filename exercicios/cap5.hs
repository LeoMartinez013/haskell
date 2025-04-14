import Data.Monoid
-- CAP5
-- EX 5.1
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show, Eq)
data Produto = Produto {valor::Double, tp::TipoProduto} | Nada deriving (Show, Eq)

instance Semigroup Produto where
    Nada <> p = p
    p <> Nada = p
    Produto v1 _ <> Produto v2 _ = Produto (v1 + v2) Total

instance Monoid Produto where
    mempty = Nada

    -- Sem o uso de monoides, seria necessário a criação manual de uma função para somar os 
    -- valores dos produtos, tratar explicitamente casos como a ausencia e garantindo que 
    -- o tipo retornado sempre use o construtor Total. tornando o código menos reutilizavel, 
    -- mais verboso e menos generico, dificultando a composição funcional e o uso direto de operações 
    -- padrão como <> ou mconcat, que facilitam combinações automaticas e elegantes quando se usa Monoid.

-- EX 5.2
totalGeral :: [Produto] -> Produto
totalGeral = mconcat

-- EX 5.3
newtype Min = Min Int deriving (Show, Eq, Ord)

instance Semigroup Min where
    Min x <> Min y = Min (min x y)

instance Monoid Min where
    mempty = Min maxBound

minimo = Min (-32) <> Min (-34) <> Min (-33) -- Min (-34)

    -- Valor mempty = Valor neutro, no caso da função 'min' deve ser o maior valor possivel, ja que qualquer
    -- outro valor deve ser menor.

-- EX 5.4
minAll :: [Min] -> Min
minAll = mconcat

-- EX 5.5
data Paridade = Par | Impar deriving (Show, Eq)
class ParImpar a where
    decide :: a -> Paridade

instance ParImpar Int where
    decide n = if even n then Par else Impar

instance ParImpar [a] where
  decide xs = decide (length xs)

instance ParImpar Bool where
  decide False = Par
  decide True  = Impar

-- EX 5.7
data Arvore a = Vazia | Folha a | Raiz a (Arvore a) (Arvore a) deriving (Show, Eq)

mapa :: (a -> b) -> Arvore a -> Arvore b
mapa _ Vazia = Vazia
mapa f (Folha x) = Folha (f x)
mapa f (Raiz x esq dir) = Raiz (f x) (mapa f esq) (mapa f dir)

-- EX 5.8
inteiros :: Arvore Int
inteiros = Raiz 1 (Folha 2) (Folha 3)
resultado = mapa (+5) inteiros

-- EX 5.11
preOrdem :: Arvore a -> [a]
preOrdem Vazia = []
preOrdem (Folha x) = [x]
preOrdem (Raiz x esq dir) = [x] ++ preOrdem esq ++ preOrdem dir

posOrdem :: Arvore a -> [a]
posOrdem Vazia = []
posOrdem (Folha x) = [x]
posOrdem (Raiz x esq dir) = posOrdem esq ++ posOrdem dir ++ [x]

-- Raiz 15 (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Vazia)) (Raiz 20 Vazia (Raiz 22 (Folha 21) Vazia))
{-
      ______15______
    __11___     __20__
    6  __12_    _   __22_
       10  _        21  _
-}{-
    preOrdem Raiz 15 esq dir
[15] ++ preOrdem esq ++ preOrdem dir

esq = Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Vazia)
preOrdem (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Vazia))
[11] ++ preOrdem (Folha 6) ++ preOrdem (Raiz 12 ...)

preOrdem (Folha 6) = [6]

preOrdem (Raiz 12 (Folha 10) Vazia)
[12] ++ preOrdem (Folha 10) ++ preOrdem Vazia
[12] ++ [10] ++ []
= [12, 10]

[11] ++ [6] ++ [12, 10]
= [11, 6, 12, 10]

dir = Raiz 20 Vazia (Raiz 22 (Folha 21) Vazia)
preOrdem (Raiz 20 Vazia (Raiz 22 (Folha 21) Vazia))
[20] ++ preOrdem Vazia ++ preOrdem (Raiz 22 ...)
[20] ++ [] ++ ([22] ++ preOrdem (Folha 21) ++ preOrdem Vazia)
[20] ++ [] ++ [22] ++ [21] ++ []
= [20, 22, 21]

Final:
[15] ++ [11, 6, 12, 10] ++ [20, 22, 21]
= [15, 11, 6, 12, 10, 20, 22, 21]
-}
{-
    posOrdem (Raiz 15 esq dir)
posOrdem esq ++ posOrdem dir ++ [15]

esq = Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Vazia)
posOrdem (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Vazia))
posOrdem (Folha 6) ++ posOrdem (Raiz 12 ...) ++ [11]

posOrdem (Folha 6) = [6]

posOrdem (Raiz 12 (Folha 10) Vazia)
posOrdem (Folha 10) ++ posOrdem Vazia ++ [12]
= [10] ++ [] ++ [12]
= [10, 12]

[6] ++ [10, 12] ++ [11]
= [6, 10, 12, 11]

dir = Raiz 20 Vazia (Raiz 22 (Folha 21) Vazia)
posOrdem (Raiz 20 Vazia (Raiz 22 (Folha 21) Vazia))
posOrdem Vazia ++ posOrdem (Raiz 22 ...) ++ [20]
[] ++ (posOrdem (Folha 21) ++ posOrdem Vazia ++ [22]) ++ [20]
[] ++ [21] ++ [] ++ [22] ++ [20]
= [21, 22, 20]

Final:
[6, 10, 12, 11] ++ [21, 22, 20] ++ [15]
= [6, 10, 12, 11, 21, 22, 20, 15]
-}