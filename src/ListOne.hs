import Data.List

-- Escreva a função calcCirc para calcular a área de um círculo de raio r. 
-- A área de um círculo é dada por:
-- area = pi * r^2

calcCirc r = pi * r^2

-- Escreva a função ehPar para verificar se um número dado é par.
-- A função deve retornar "Par!" ou "Impar!".

isEven :: Int -> String
isEven n
  | (mod n 2 == 0) = "Par" 
  | otherwise = "Impar"


-- Escreva uma função que recebe um valor numérico e devolva o valor 1 
-- se o valor for maior que zero, -1 se for negativo, 0 se for zero.

biggerZero :: Float -> Int
biggerZero number
  | number > 0 = 1
  | number < 0 = (-1)
  | otherwise = 0

-- Defina uma função que receba três valores inteiros e 
-- retorne o menor deles.

getSmaller :: Int -> Int -> Int -> Int
getSmaller a b c 
  | (a < b && a < c) = a
  | (b < a && b < c) = b
  | otherwise = c

-- Escreva uma função que receba três valores booleanos e retorna a 
-- operação AND aplicada aos três.

threeAndBooleans :: Bool -> Bool -> Bool -> Bool
threeAndBooleans a b c = a && b && c

-- Escreva uma função que receba três valores booleanos e retorna a 
-- operação OR aplicada aos três.

threeOrBooleans :: Bool -> Bool -> Bool -> Bool
threeOrBooleans a b c = a || b || c


-- Escreva uma função que receba dois valores booleanos e retorne 
-- a operação XOR (OU exclusivo) aplicada aos dois.

xor :: Bool -> Bool -> Bool
xor x y  = (x || y) && not (x && y)

-- Defina o operador # que concatena duas strings caso elas 
-- sejam diferentes. Se forem iguais retorna uma das duas.

hasTag :: [Char] -> [Char] -> String
hasTag first second 
  | first == second = first
  | otherwise = first ++ second

operations:: Char -> Integer -> Integer -> (Integer, String)
operations op n1 n2
  | op == '*' = (n1 * n2, "Operation valid")
  | op == '/' = (div n1 n2, "Operation valid")
  | op == '-' = (n1 - n2, "Operation valid")
  | op == '+' = (n1 + n2, "Operation valid")
  | otherwise = ((-1), "Invalid option")


-- Considere que o preço de uma passagem de avião em um trecho pode variar
-- dependendo a idade do passageiro. Pessoas com 60 anos ou mais pagam apenas
-- 60% do preço total. Crianças até 10 anos pagam 50% e bebês (abaixo de 2 anos)
-- pagam apenas 10%. Faça uma função que tenha como entrada o valor total da
-- passagem e a idade do passageiro e produz o valor a ser pago.

applyOffer :: Float -> Int -> Float
applyOffer priceTotal age
  | age > 60 = priceTotal - (priceTotal * 0.60)
  | age <= 10 = priceTotal / 2
  | otherwise = priceTotal