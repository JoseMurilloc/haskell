import Data.List


{--
    1) Defina a função formaFrase :: [String] -> String, que recebe uma lista de strings e retorna uma
frase com as strings separadas por ',' (vírgula), sendo que a última é separada por 'e'.
--}

isPenultimate :: [String] -> Bool
isPenultimate (x:[]) = True
isPenultimate (x:xs) = False


formatWorld :: [String] -> String
formatWorld (x:[]) = "e " ++ x
formatWorld (x:xs)
    | isPenultimate xs = x ++ " " ++ formatWorld xs 
    | otherwise = x ++ ", " ++ formatWorld xs
{--
    2) Crie uma função que receba como parâmetro um número inteiro (n) e retorne uma lista com
os n primeiros números da sequência de fibonacci. Considere o inicio da contagem a partir de
zero, conforme exemplo a seguir.
--}

calcFib :: Integer -> Integer
calcFib 0 = 0
calcFib 1 = 1
calcFib n = calcFib (n-1) + calcFib (n-2)


seqFib :: Integer -> [Integer] 
seqFib 0 = []
seqFib number = map calcFib [0..number]


{--
    3) Crie a função perfeito :: Int -> Bool que recebe um número inteiro e retorna se ele é ou não
perfeito (True/False) . Um número é considerado perfeito se a soma dos seus divisores, exceto
ele, é igual a próprio número. Por exemplo, o número seis (6) é perfeito, pois a soma dos seus
divisores é igual a ele mesmo: 1+2+3 = 6.
--}

getAllDivisors :: Int -> [Int]
getAllDivisors number = [x | x <- [1..number], (checkDivisor number x), x /= number]

checkDivisor :: Int -> Int -> Bool
checkDivisor number divisor 
    | mod number divisor == 0 = True
    | otherwise = False


numberIsPerfect :: Int -> Bool
numberIsPerfect number
    | sum (getAllDivisors number) == number = True
    | otherwise = False


{--
    4) A ordenação por inserção tem como proposta ordenar uma lista reconstruindo-a de forma
ordenada a partir de uma lista vazia. Portanto, inicia-se com uma lista vazia e os elementos da
lista recebida como parâmetro são inseridos já ordenados, um de cada vez. Crie a função
insertionSort :: [Int] -> [Int] que recebe uma lista de inteiros e retorne a lista devidamente
ordenada por inserção.
--}

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort list = sort list