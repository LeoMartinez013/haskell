## 6.1 CATEGORIAS
Uma categoria C é uma estrutura abstrata composta de objetos e morfismos. Um objeto é uma representação abstrata do que se
quer estudar (conjuntos, tabelas, palavras, pessoas, animais etc.) e um morfismo é uma relação de objetos que se compõem de forma associativa. Existe também o morfismo neutro, chamado identidade. São coleções simples com três componentes:

- Coleção de todos os objetos chamada de ob( C ) .
- Coleção de todos os morfismos chamada de hom( C ) : um morfismo relaciona dois objetos A e B. Um morfismo f :: A -> B relaciona um objeto de entrada A com um de saída B (generalização do conceito de função).
- Noção de composição dos morfismos: (.): Se g :: A -> B e f :: B -> C , então f.g :: A -> C . Em toda categoria, é necessário existir a função identidade. Em outras palavras, para cada objeto X de uma categoria C, existe um morfismo idX :: X -> X , tal que para cada morfismo f :: A -> B satisfaça: 

```haskell
f . idA = f = idB . f
```