-- 2.1 Gere listas
-- a) [1,11,121,1331,14641,161051,1771561]
-- ListaA :: [Int]
listaA = [11^n | n <- [0..6]]

-- b) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]
-- ListaB :: [Int]
listaB = [x | x <- [1..40], x `mod` 4 /= 0]

-- c) ["AaBB", "AbBB", "AcBB", "AdB-B", "AeBB", "AfBB","AgBB"]
listaC :: [String]
listaC = ["A" ++ [c] ++ "BB" | c <- ['a'..'g']]

-- d) [5,8,11,17,20,26,29,32,38,41]
-- listaD :: [Int]
listaD = scanl (+) 5 [3,3,6,3,6,3,3,6,3]

-- e) [1.0,0.5,0.25,0.125,0.0625,0.03125]
-- ListaE :: [Int]
listaE = [1 / 2^n | n <- [0..5]]

-- f) [1,10,19,28,37,46,55,64]
-- ListaF :: [Int]
listaF = [1 + 9*n | n <- [0..7]]

-- g) [2,4,8,10,12,16,18,22,24,28,30]
listaG :: [Int]
listaG = scanl (+) 2 [2,4,2,2,4,2,4,2,4,2]

-- h) ['@','A','C','D','E','G','J','L']
-- ListaH :: [String]
listaH :: [String]
listaH = map (:[]) "@ACDEGJL"
-- listaH = map (\c -> [c]) ['@','A','C','D','E','G','J','L']

--  2.2 Crie uma função que verifique se o tamanho de uma String é par ou não. Use Bool como retorno.
isPar :: String -> Bool
isPar = even . length

--  2.3 Escreva uma função que receba um vetor de Strings e retorne uma lista com todos os elementos em ordem reversa.
reverseList :: [String] -> [String]
reverseList = reverse

--  2.4 Escreva uma função que receba um vetor de Strings e retorne uma lista com o tamanho de cada String. As palavras de tamanho par devem ser excluídas da resposta.
sizeStrings :: [String] -> [Int]
sizeStrings = map length . filter (odd . length)

--  2.5 Escreva a função head como composição de duas outras.
-- Interpetramos que devemos criar uma função semelhante a função nativa "head"
funcHead :: [a] -> a
funcHead = last . take 1

-- Outro jeito que descobrimos que lida com listas vazias, não sei ainda se é utilizavel mas parece legal
funcSafeHead :: [a] -> Maybe a
funcSafeHead []    = Nothing
funcSafeHead (x:_) = Just x


--  2.6 Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário, False.
verifPalindromo :: String -> Bool
verifPalindromo s = s == reverse s
-- isPalindrome = (==) <*> reverse

--  2.7 Faça uma função que receba um inteiro e retorne uma tupla, contendo: o dobro deste número na primeira coordenada, o triplo na segunda, o quádruplo na terceira e o quíntuplo na quarta.
multiplos :: Int -> (Int, Int, Int, Int)
multiplos n = (2*n, 3*n, 4*n, 5*n)

-- 3.1 CRIE O TIPO PERGUNTA COM OS VALUES CONSTRUCTORS SIM OU NAO. FAÇA AS FUNÇÕES SEGUINTES, DETERMINANDO SEUS TIPOS EXPLICITAMENTE.
-- pergNum : recebe via parâmetro uma Pergunta . Retorna 0 para Nao e 1 para Sim .
-- listPergs : recebe via parâmetro uma lista de Perguntas , e retorna 0 s e 1 s correspondentes aos constructores contidos na lista.
-- and' : recebe duas Perguntas como parâmetro e retorna a tabela verdade do and lógico, usando Sim como verdadeiro e Nao como falso.
-- or' : idem ao anterior, porém deve ser usado o ou lógico.
-- not' : idem aos anteriores, porém usando o not lógico.
-- data Pergunta = Sim | Nao deriving Show

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

-- 3.2 FAÇA O TIPO TEMPERATURA QUE PODE TER VALORES CELSIUS , FARENHEIT OU KELVIN . IMPLEMENTE AS FUNÇÕES:
-- converterCelsius : recebe um valor double e uma temperatura, e faz a conversão para Celsius.
-- converterKelvin : recebe um valor double e uma temperatura, e faz a conversão para Kelvin.
-- converterFarenheit : recebe um valor double e uma temperatura, e faz a conversão para Farenheit.
-- data Temperatura = Kelvin Double | Celsius Double | Farenheit Double deriving Show

paraCelsius :: Temperatura -> Double
paraCelsius (Celsius t) = t
paraCelsius (Farenheit t) = (t - 32) * 1.8
paraCelsius (Kelvin t) = t - 273.15

paraKelvin :: Temperatura -> Double
paraKelvin t = paraCelsius t + 273.15

paraFarenheit :: Temperatura -> Double
paraFarenheit t = paraCelsius t * 1.8 + 32

-- 3.3 IMPLEMENTE UMA FUNÇÃO QUE SIMULE O VENCEDOR DE UMA PARTIDA DE PEDRA, PAPEL E TESOURA USANDO TIPOS CRIADOS. CASOS DE EMPATE DEVEM SER CONSIDERADOS EM SEU TIPO.
data Jokenpo = Pedra | Papel | Tesoura deriving (Show, Eq)
data Resultado = VitoraJogador1 | VitoraJogador2 | Empate deriving (Show, Eq)

jogar :: Jokenpo -> Jokenpo -> Resultado
jogar Pedra Tesoura = VitoraJogador1
jogar Tesoura Papel = VitoraJogador1
jogar Papel Pedra   = VitoraJogador1
jogar j1 j2 | j1 == j2  = Empate | otherwise = VitoraJogador2

-- 3.4 FAÇA UMA FUNÇÃO QUE RETORNE UMA STRING, COM TODAS AS VOGAIS MAIÚSCULAS E MINÚSCULAS ELIMINADAS DE UMA STRING PASSADA POR PARÂMETRO USANDO LIST COMPREENSHION.
consoantes :: String -> String
consoantes xs = [ x | x <- xs, x `notElem` "aeiouAEIOU"]

-- 3.5 Sabe-se que as unidades imperiais de comprimento podem ser Inch , Yard ou Foot (há outras ignoradas aqui). Sabe-se que 1in=0.0254m , 1yd=0.9144m , 1ft=0.3048 . Faça a função converterMetros que recebe a unidade imperial e o valor correspondente nesta unidade. Esta função deve retornar o valor em metros.
-- Implemente também a função converterImperial , que recebe um valor em metros e a unidade de conversão. Esta função deve retornar o valor convertido para a unidade desejada. data Imperial = Inch Double | Yard Double | Foot Double deriving Show

converterMetros :: Imperial -> Double
converterMetros (Inch l) = l*0.0254
converterMetros (Yard l) = l*0.9144
converterMetros (Foot l) = l*0.3048

converterImperial :: Double -> Imperial -> Double
converterImperial l (Inch _) = l / converterMetros (Inch 1)
converterImperial l (Yard _) = l / converterMetros (Yard 1)
converterImperial l (Foot _) = l / converterMetros (Foot 1)

-- 3.6 FAÇA UM NOVO TIPO CHAMADO MES , QUE POSSUI COMO VALORES TODOS OS MESES DO ANO. IMPLEMENTE:
-- A FUNÇÃO CHECAFIM , QUE RETORNA O NÚMERO DE DIAS QUE CADA MÊS POSSUI (CONSIDERE FEVEREIRO TENDO 28 DIAS).
-- A FUNÇÃO PROX , QUE RECEBE UM MÊS ATUAL E RETORNA O PRÓXIMO MÊS.
-- A FUNÇÃO ESTACAO , QUE RETORNA A ESTAÇÃO DO ANO DE ACORDO COM o mês e com o hemisfério.
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

-- 3.7 FAÇA UMA FUNÇÃO QUE RECEBA UMA STRING E RETORNE TRUE SE ESTA FOR UM PALÍNDROMO; CASO CONTRÁRIO, FALSE .
palimdromo :: String -> Bool
palimdromo s = s == reverse s

