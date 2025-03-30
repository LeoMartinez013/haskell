-- CAP2 2.1 Gere listas
-- a) [1,11,121,1331,14641,161051,1771561]
-- ListaA :: [Int]
listaA = [11^n | n <- [0..6]]

-- b) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]
-- ListaB :: [Int]
listaB = [x | x <- [1..40], x `mod` 4 /= 0]

-- c) ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB","AgBB"]
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

-- CAP2 2.2 Crie uma função que verifique se o tamanho de uma String é par ou não. Use Bool como retorno.
isPar :: String -> Bool
isPar = even . length

-- CAP2 2.3 Escreva uma função que receba um vetor de Strings e retorne uma lista com todos os elementos em ordem reversa.
reverseList :: [String] -> [String]
reverseList = reverse

-- CAP2 2.4 Escreva uma função que receba um vetor de Strings e retorne uma lista com o tamanho de cada String. As palavras de tamanho par devem ser excluídas da resposta.
sizeStrings :: [String] -> [Int]
sizeStrings = map length . filter (odd . length)

-- CAP2 2.5 Escreva a função head como composição de duas outras.
-- Eu interpetrei que eu devo criar uma função semelhante a função nativa "head"
funcHead :: [a] -> a
funcHead = last . take 1

-- Outro jeito que descobri que lida com listas vazias, não sei ainda se é utilizavel mas parece legal
funcSafeHead :: [a] -> Maybe a
funcSafeHead []    = Nothing
funcSafeHead (x:_) = Just x


-- CAP2 2.6 Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário, False.
verifPalindromo :: String -> Bool
verifPalindromo s = s == reverse s
-- isPalindrome = (==) <*> reverse

-- CAP2 2.7 Faça uma função que receba um inteiro e retorne uma tupla, contendo: o dobro deste número na primeira coordenada, o triplo na segunda, o quádruplo na terceira e o quíntuplo na quarta.
multiplos :: Int -> (Int, Int, Int, Int)
multiplos n = (2*n, 3*n, 4*n, 5*n)
