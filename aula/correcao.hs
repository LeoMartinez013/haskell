-- a
qtdMaiusculas :: [char] -> Int
qtdMaiusculas xs = length [x | x <- xs, elem x ['A'..'Z']]

-- b
somarTodos :: [Int] -> Int
somarTodos xs = foldl (\ b a -> b + a) 0 $ filtrarPositivos xs
filtrarPositivos :: [Int] -> Int
filtrarPositivos = filter (>=0)

-- c
qtdCarB :: String -> Int
qtdCarB xs = length $ filter (\ x -> x == 'B') xs
    
-- d
somarDouble :: [Double] -> Double
somarDouble xs = foldl (\ b a -> b + a) 0 $ filtrarPositivos xs
