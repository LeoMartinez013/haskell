-- CAP3 
-- 3.1 
data Pergunta = Sim | Nao deriving Show

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs [] = []
listPergs (x:xs) = pergNum x : listPergs xs

pergToBool :: Pergunta -> Bool
pergToBool Nao = False
pergToBool Sim = True

and' :: Pergunta -> Pergunta -> [Bool]
and' x y = [pergToBool x, pergToBool y, pergToBool x && pergToBool y]

or' :: Pergunta -> Pergunta -> [Bool]
or' x y = [pergToBool x, pergToBool y, pergToBool x || pergToBool y]

not' :: Pergunta -> Bool
not' x = not (pergToBool x)

-- 3.2
data Temperatura = Kelvin Double | Celsius Double | Farenheit Double deriving Show

paraCelsius :: Temperatura -> Double
paraCelsius (Celsius t) = t
paraCelsius (Farenheit t) = (t - 32) * 1.8
paraCelsius (Kelvin t) = t - 273.15

paraKelvin :: Temperatura -> Double
paraKelvin t = paraCelsius t + 273.15

paraFarenheit :: Temperatura -> Double
paraFarenheit t = paraCelsius t * 1.8 + 32

-- 3.3
data Jokenpo = Pedra | Papel | Tesoura deriving (Show, Eq)
data Resultado = VitoraJogador1 | VitoraJogador2 | Empate deriving (Show, Eq)

jogar :: Jokenpo -> Jokenpo -> Resultado
jogar Pedra Tesoura = VitoraJogador1
jogar Tesoura Papel = VitoraJogador1
jogar Papel Pedra   = VitoraJogador1
jogar j1 j2 | j1 == j2  = Empate | otherwise = VitoraJogador2

-- 3.4
consoantes :: String -> String
consoantes xs = [ x | x <- xs, x `notElem` "aeiouAEIOU"]

-- 3.5
data Imperial = Inch Double | Yard Double | Foot Double deriving Show

converterMetros :: Imperial -> Double
converterMetros (Inch l) = l*0.0254
converterMetros (Yard l) = l*0.9144
converterMetros (Foot l) = l*0.3048

converterImperial :: Double -> Imperial -> Double
converterImperial l (Inch _) = l / converterMetros (Inch 1)
converterImperial l (Yard _) = l / converterMetros (Yard 1)
converterImperial l (Foot _) = l / converterMetros (Foot 1)

-- 3.6
data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho 
    | Julho | Agosto | Setembro | Novembro | Outubro | Dezembro deriving (Show, Eq)

checaFim :: Mes -> Int
checaFim Janeiro    = 31
checaFim Fevereiro  = 28
checaFim Marco      = 31
checaFim Abril      = 30
checaFim Maio       = 31
checaFim Junho      = 30
checaFim Julho      = 31
checaFim Agosto     = 31
checaFim Setembro   = 30
checaFim Outubro    = 31
checaFim Novembro   = 30
checaFim Dezembro   = 31

prox :: Mes -> Mes
prox Janeiro   = Fevereiro
prox Fevereiro = Marco
prox Marco     = Abril
prox Abril     = Maio
prox Maio      = Junho
prox Junho     = Julho
prox Julho     = Agosto
prox Agosto    = Setembro
prox Setembro  = Outubro
prox Outubro   = Novembro
prox Novembro  = Dezembro
prox Dezembro  = Janeiro

data Hemisferio = Norte | Sul
  deriving Show

data Estacao = Verao | Outono | Inverno | Primavera
  deriving Show

estacao :: Mes -> Hemisferio -> Estacao
estacao mes Norte
  | mes `elem` [Dezembro, Janeiro, Fevereiro] = Inverno
  | mes `elem` [Marco, Abril, Maio]           = Primavera
  | mes `elem` [Junho, Julho, Agosto]           = Verao
  | mes `elem` [Setembro, Outubro, Novembro]    = Outono
estacao mes Sul
  | mes `elem` [Dezembro, Janeiro, Fevereiro] = Verao
  | mes `elem` [Marco, Abril, Maio]           = Outono
  | mes `elem` [Junho, Julho, Agosto]           = Inverno
  | mes `elem` [Setembro, Outubro, Novembro]    = Primavera

-- 3.7
palimdromo :: String -> Bool
palimdromo s = s == reverse s