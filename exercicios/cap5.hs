import Data.Monoid
-- CAP5
-- EX 5.1
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show, Eq)
data Produto = Produto {valor::Double, tp::TipoProduto} | Nada deriving (Show, Eq)

instance Semigroup Produto where
    Nada <> p = p
    p <> Nada = p
    Produto v1 _ <> Produto v2 _ = Produto (v1 + v2) Total

instance Monoid Produto where
    mempty = Nada

    -- Sem o uso de monoides, seria necessário a criação manual de uma função para somar os 
    -- valores dos produtos, tratar explicitamente casos como a ausencia e garantindo que 
    -- o tipo retornado sempre use o construtor Total. tornando o código menos reutilizavel, 
    -- mais verboso e menos generico, dificultando a composição funcional e o uso direto de operações 
    -- padrão como <> ou mconcat, que facilitam combinações automaticas e elegantes quando se usa Monoid.

-- EX 5.2
totalGeral :: [Produto] -> Produto
totalGeral = mconcat

-- EX 5.3
newtype Min = Min Int deriving (Show, Eq, Ord)

instance Semigroup Min where
    Min x <> Min y = Min (min x y)

instance Monoid Min where
    mempty = Min maxBound

minimo = Min (-32) <> Min (-34) <> Min (-33) -- Min (-34)

    -- Valor mempty = Valor neutro, no caso da função 'min' deve ser o maior valor possivel, ja que qualquer
    -- outro valor deve ser menor.

-- EX 5.4
minAll :: [Min] -> Min
minAll = mconcat
-- EX 5.5
-- EX 5.7
-- EX 5.8
-- EX 5.11