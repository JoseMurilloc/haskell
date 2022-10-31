
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- -- -- -- -- -- -- -- -- LAMBDA STUDANT -- -- -- -- -- -- -- -- -- -- 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

foldList :: [Int] -> [Int] 
foldList ls = map fold ls
    where fold x = 2 * x

-- Sintaxe lambda (array function pass how params)
dobrarLista ls = map (\x -> 2 * x) ls


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- -- -- -- -- -- -- -- -- -- QUESTION -- -- -- -- -- -- -- -- -- -- -- 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

-- 1. Crie expressões lambda para:
-- a. Calcular o dobro de um número.
foldNumber = (\x -> 2 * x)  
-- b. Verificar se um número é par.
numberIsEven = (\x -> (mod x 2) == 0)  
-- c. Receber três argumentos e calcula a sua soma.
sumThreeNumbers = (\x y z -> x + y + z)  
-- d. Receber uma tupla e retornar a soma dos elementos que compõem a tupla.
sumTuple = (\(x,y) -> x + y)
-- e. Calcular o fatorial de um número.
-- calcFat = map (\x -> x * calcFat (x-1)) [x..1]
calcFat :: Int -> Int
calcFat 0 = 1
calcFat n = lambdaCalcFat n 

lambdaCalcFat = (\x -> x * calcFat (x - 1))
-- f. Determinar se um valor pertence a uma lista.
lambdaContainInList = (\v xs -> containInList v xs)

containInList :: Int -> [Int] -> Bool
containInList v [] = False
containInList v (x:xs) 
    | x == v = True
    | otherwise = lambdaContainInList v xs

-- g. Concatenar duas listas.
concatList = (\listOne  listTwo -> listOne ++ listTwo)
-- h. Dada uma lista, retornar outra com os elementos em ordem inversa.

sortList :: [Int] -> [Int]
sortList [] = []
sortList (x:y:xs) 
    | x < y = x:(xs)
    | otherwise = sortList (y:xs)

