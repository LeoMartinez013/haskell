# Correção


1. 
```haskell
zar' (\ x -> x == 'S' == 'F' || 'S' == 'E') "antos" = 
zar' (\ x -> x == 'A' == 'F' || 'A' == 'E') "ntos" = 
zar' (\ x -> x == 'N' == 'F' || 'N' == 'E') "tos" = 
zar' (\ x -> x == 'T' == 'F' || 'T' == 'E') "os" = 
zar' (\ x -> x == 'O' == 'F' || 'O' == 'E') "s" = 
zar' (\ x -> x == 'S' == 'F' || 'S' == 'E') "" = []
```

2. 
```haskell
qtdMaiusculas :: [char] -> Int
qtdMaiusculas xs = len
```