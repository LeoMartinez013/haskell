-- Pattern Matching

numerosExtenso :: Int -> String
numerosExtenso 1 = "Um"
numerosExtenso 2 = "Dois"
numerosExtenso x = "?"

saudar :: String ->String
saudar "Fulano" = "Seja bem vindo Fulano"
saudar "Cicrano" = "Seja bem vindo Cicrano"
saudar "Palmeiras" = "Nao tem mundial"
saudar nome = "Seja bem vindo: " ++ nome

pmTupla :: (Int,Int) -> Int
pmTupla (1,1)   = 2
pmTupla (x,1)   = 1
pmTupla (1,x)   = x
pmTupla (321,4) = 326
pmTupla (x, y)  = x + y

pmLista :: [Int] -> Int
pmLista []      = 0
pmLista [x]     = x
pmLista [y,_]   = y
pmLista [z,y,x] = z
pmLista (x:xs)  = x
-- pmLista [1,2,3,4,5,6...] retorna 1
pmLista (x:z:y:zs) = x
-- pmLista [1,2,3,4,5,6...] retorna 1 apenas quando tiver 3 elementos

-- Tipos de dados algébricos

data Binario = Zero | Um deriving Show

binarioParaInt ::  Binario -> Int
binarioParaInt Um = 1
binarioParaInt Zero = 0

intParaBinario 1 = Um
intParaBinario 0 = Zero

{-
data Pessoa = Fisica String Int | Juridica String Int deriving Show

getNome :: Pessoa -> String
getNome (Fisica nome cpf) = nome
getNome (Juridica nome cnpj) = nome

setNome :: Pessoa -> String -> Pessoa
setNome (Fisica nome cpf) novoNome = Fisica novoNome cpf
-}

-- Record Syntax
data Pessoa = Fisica{nome::String, cpf::String} | Juridica{nome::String, cnpj::String} deriving Show