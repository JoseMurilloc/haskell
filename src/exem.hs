{-
    1) Crie uma função que, dada uma lista de inteiros e um inteiro qualquer,
    retorne todos os inteiros que não são divisíveis por esse inteiro passado
    como parâmetro.
    Exemplos:
--}

checkDivisor :: Int -> Int -> Bool
checkDivisor number divisor 
    | mod number divisor == 0 = True
    | otherwise = False

cousinBetween :: [Int] -> Int -> [Int]
cousinBetween (x:[]) n
    | not (checkDivisor n x) = [x]
    | otherwise = []
cousinBetween (x:xs) n
    | not (checkDivisor x n) = x:[] ++ (cousinBetween xs n)
    | otherwise = cousinBetween xs n