-- Crie funções para resolver os seguintes problemas:
-- a. Remover o primeiro elemento de uma lista.
fetchFirstElement:: [Int] -> Int
fetchFirstElement list = head list

-- b. Remover a primeira ocorrência de um dado elemento em uma lista.
removeFirstElement :: Int -> [Int] -> [Int]
removeFirstElement element (x:xs)
  | element == x = xs
  | otherwise = [x] ++ removeFirstElement element xs

-- c. Remover o último elemento de uma lista.
removeLastElement :: [Int] -> Int
removeLastElement (x:[]) = x
removeLastElement (x:xs) = 0 + removeLastElement xs

-- d. Concatenar duas listas.
joinTwoList :: [Int] -> [Int] -> [Int]
joinTwoList l1 l2 = l1 ++ l2

-- e. Retornar o maior elemento de uma lista
getBiggerElement :: [Int] -> Int
getBiggerElement [] = 0
getBiggerElement (x:[]) = x
getBiggerElement (x:xs)
  | x > getBiggerElement xs = x
  | otherwise = getBiggerElement xs

-- f. Retornar o menor elemento de uma lista
getSmallerElement :: [Int] -> Int
getSmallerElement [] = 0
getSmallerElement (x:[]) = x
getSmallerElement (x:xs)
  | x < getSmallerElement xs = x
  | otherwise = getSmallerElement xs

-- g. Retornar o número de elementos de uma lista.
getSizeOfList :: [Int] -> Int
getSizeOfList [] = 0
getSizeOfList (x:[]) = 1
getSizeOfList (x:xs) = 1 + getSizeOfList xs

-- h. Verificar se um dado elemento pertence a uma lista.
belongs :: Int -> [Int] -> Bool
belongs item [] = False
belongs item (x:xs)
  | x == item = True
  | otherwise = belongs item xs   

-- i. Receber uma lista e um número inteiro positivo n e retornar o n-ésimo
-- termo da lista.

-- 2. Implemente uma função proditório que, dada uma lista de números de ponto flutuante, calcular o produto de todos esses números.
-- Exemplo:
-- Main > proditório [3, 4, 6, 2]
-- 144
amountProducts :: [Float] -> Float
amountProducts (x:[]) = x 
amountProducts (x:xs) = x * amountProducts xs


-- 3. Crie uma função que, dada uma lista de trios de números devolve uma lista com
-- as somas dos elementos desses trios:
-- Exemplo:
-- Main > somaElementos [(1,2,3), (3,4,5), (5,6,7), (7,8,9), (9, 10, 11)]
-- [6, 12, 18, 24, 30]
type Element = (Int, Int, Int)
sumElements :: [Element] -> [Int]
sumElements (x:[]) = sumTuple x
  where sumTuple (x1, x2, x3) = [x1+x2+x3]
sumElements (x:xs) = sumTuple x ++  sumElements xs
  where sumTuple (x1, x2, x3) = [x1+x2+x3]


--   4. A partir de dois argumentos, um valor inteiro n e uma tupla (a,b), construa uma
-- função que retorne quais são os múltiplos de n no intervalo entre a e b.
-- Exemplo:
-- Main> multiplos 3 (1, 30)
-- [3,6,9,12,15,18,21,24,27,30]
multiplesBetween :: Int -> (Int, Int) -> [Int]
multiplesBetween element (a,b) = [x | x <- [a..b], mod x element == 0]

{-
  5. Crie uma função que, dada uma lista de inteiros e um inteiro qualquer, retorne todos os inteiros pertencentes a essa lista que não são divisíveis por esse inteiro
  passado como parâmetro.
  Exemplo:
  Main > primosEntreSi [5,8,10,32,56,73] 4
  [5,10,73]
-}

cousinsBetween :: [Int] -> Int -> [Int]
cousinsBetween (x:xs) cousin = [x | x <- (x:xs), mod x cousin /= 0]

{-
6. Defina uma função apenasPrimos que, dada uma lista de números inteiros,
devolve uma lista contendo apenas aqueles da lista original que são primos.
Exemplo:
Main > apenasPrimos [6,8,3,78,43,86,4,307,24,67,8,435,234,101]
[3,43,307,67,101]
-}

onlyCousin :: [Int] -> [Int]
onlyCousin (x:xs) = [x | x <- (x:xs), isCourse x]

isCourse :: Int -> Bool
isCourse n = checkAmountDiv([x | x <- [1..n], mod n x == 0])
  where checkAmountDiv l = length l == 2

-- 7. Construa uma função que converta um inteiro dado como argumento no número
-- na base binária correspondente.
-- Exemplo:
-- Main> binário 34
-- "100011"
