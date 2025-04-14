import Distribution.Simple.Utils (xargs, isAbsoluteOnAnyPlatform)
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