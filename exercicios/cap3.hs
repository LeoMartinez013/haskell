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

