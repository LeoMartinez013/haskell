somar :: Int -> Int -> Int
somar x y = x + y

saudacao :: String -> [Char]
saudacao nome = "Ola: " ++ nome ++ ". Seja bem vind."

impares :: [Int] -> [Int]
impares xs = [ x | z<-zs, odd x]