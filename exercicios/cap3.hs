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