-- CAP4
-- EX 4.3
numPar :: [Int] -> [Int]
numPar [] = []
numPar x = filter even x

numImpar :: [Int] -> [Int]
numImpar [] = []
numImpar x = filter odd x

-- EX 4.4
ehPrimo :: Int -> Bool
ehPrimo n
  | n < 2     = False
  | otherwise = null [x | x <- [2..(floor . sqrt $ fromIntegral n)], n `mod` x == 0]

primos :: [Int] -> [Int]
primos [] = []
primos x = filter ehPrimo x

-- EX 4.6
func :: (String -> String) -> String -> String
func f s = reverse s ++ f s

-- EX 4.7
data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Show, Eq)

tercas :: [Dia] -> [Dia]
tercas [] = []
tercas (x:xs)
    | x == Terca = x : tercas xs
    | otherwise = tercas xs

-- EX 4.8
data Correncia = Real | Dolar deriving (Show, Eq)
data Dinheiro = Dinheiro {valor::Double, correncia::Correncia} deriving (Show, Eq)

paraDolar :: [Dinheiro] -> [Dinheiro]
paraDolar [] = []
paraDolar (x:xs)
    | correncia x == Dolar = x : paraDolar xs
    | otherwise = Dinheiro (valor x * 0.1704) Dolar : paraDolar xs

paraReal  :: [Dinheiro] -> [Dinheiro]
paraReal [] = []
paraReal (x:xs)
    | correncia x == Real = x : paraReal xs
    | otherwise = Dinheiro (valor x / 0.1704) Real : paraReal xs

dolares :: [Dinheiro] -> [Dinheiro]
dolares [] = []
dolares x = filter (\ x -> correncia x == Dolar ) x

sumDolares :: [Dinheiro] -> Double
sumDolares x = sum (map valor(dolares x))

contDolares :: [Dinheiro] -> Int
contDolares x = length $ dolares x

-- EX 4.9
contNegativos :: [Int] -> Int
contNegativos = foldl (\i x -> i + if x < 0 then 1 else 0) 0

contP :: String -> Int
contP = foldl (\i x -> i + if x == 'p' || x == 'P' then 1 else 0) 0

-- Reaproveitando Dia do ex 4.7
contSabado :: [Dia] -> Int
contSabado = foldl (\ i x -> i + if x == Sabado then 1 else 0) 0

diaParaInt :: Dia -> Int
diaParaInt x
    | x == Segunda  = 1
    | x == Terca    = 2
    | x == Quarta   = 3
    | x == Quinta   = 4
    | x == Sexta    = 5
    | x == Sabado   = 6
    | x == Domingo  = 7

somaDia :: [Dia] -> Int
somaDia = foldl (\ i x -> i + diaParaInt x) 0
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