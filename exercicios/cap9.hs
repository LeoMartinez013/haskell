import System.Random
-- 9.1
main1 :: IO ()
main1 = do
    putStr "Digite um número: "
    input <- getLine
    let numero = (read input :: Int)
    if even numero 
        then putStrLn "O número é par."
        else putStrLn "O número é ímpar."

-- 9.2
main2 :: IO ()
main2 = do
    putStr "Digite uma palavra: "
    input <- getLine
    putStrLn $ reverse input

-- 9.3
data Jogada = Pedra | Papel | Tesoura deriving (Show, Eq, Read, Enum, Bounded)
data Resultado = Vitoria | Derrota | Empate deriving (Show, Eq)
resolver :: Jogada -> Jogada -> Resultado
resolver Pedra Tesoura = Vitoria
resolver Papel Pedra = Vitoria
resolver Tesoura Papel = Vitoria
resolver x y | x == y = Empate
resolver _ _ = Derrota

main3 :: IO ()
main3 = do
    putStrLn "Escolha sua Jogada (Pedra, Papel ou Tesoura):"
    jogadaUsuario <- readLn :: IO Jogada
    let min = fromEnum (minBound :: Jogada)
    let max = fromEnum (maxBound :: Jogada)
    jogadaCompEnum <- randomRIO (min, max)
    let jogadaComp = toEnum jogadaCompEnum :: Jogada
    
    putStrLn $ "Jogada do Computador: " ++ show jogadaComp
    let res = resolver jogadaUsuario jogadaComp
    putStrLn $ "Resultado: " ++ show res


-- 9.8
main8 :: IO ()
main8 = do
    let entrada = "9.8.txt"
    let saida = "9.8_Resultado.txt"
    arq <- readFile entrada
    let linhas = lines arq
    let maiores = map (\ x -> maximum (map read (words x) :: [Double])) linhas
    let maior = maximum maiores
    let textoMaiores = unlines $ map show maiores
    let textoMaior = show maior
    writeFile saida (textoMaiores ++ "\nMaior: " ++ textoMaior)
