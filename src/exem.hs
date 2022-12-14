import Data.List

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


{--
    2) Defina uma função inserirElemento :: [a] - > a - > Int - > [a] , que recebe
    uma lista de elementos, um elemento e um inteiro, inserindo o elemento
    recebido na posição dada pelo inteiro, retornando a lista final. Considere
    que 1 é a primeira posição e não serão passadas entradas com a posição menor
    do que 1.
        Exemplo
        Prelude> inserirElemento ['a', 'b', 'c'] 'x' 2
        ['a', 'x', 'b' , 'c' ]
--}



insertElement :: [a] -> a -> Int -> [a]
insertElement [] newElement _ = [newElement]
insertElement (x:xs) newElement i 
  | i <= 0 = newElement:x:xs
  | otherwise = x : insertElement xs newElement (i - 1)

{--
    3) Defina uma função eliminarRepetidos :: [ Int ] ->  [ Int ] , que recebe uma
    lista de inteiros e devolve a mesma lista, mas sem elementos repetidos,
    mantendo apenas a primeira ocorrência de cada elemento.
    Exemplo
    Prelude> eliminarRepetidos [1,2,1,2,3,4,5,3,7]
    [1,2,3,4,5,7]
--}

verifyAmountElement :: [Int] -> Int -> Int
verifyAmountElement [] verify = 0
verifyAmountElement (x:xs) verify
    | x == verify = 1 + verifyAmountElement xs verify
    | otherwise = verifyAmountElement xs verify



deleteRepeatedElements :: [Int] -> [Int]
deleteRepeatedElements [] = []
deleteRepeatedElements (x:[]) = [x]
deleteRepeatedElements (x:xs)
    | verifyAmountElement (x:xs) x < 2 = x:[] ++ deleteRepeatedElements xs
    | otherwise = deleteRepeatedElements xs


{--
    4) Construa uma função que recebe uma lista de nomes e retorna uma tupla com o
        nome e a quantidade de ocorrências deste nome nesta lista.
        Exemplo:
            Prelude> occurrences ["macaco", "macaco", "girafa", "zebra", "zebra",
            "zebra", "macaco", "girafa"]
            [("macaco",3),("girafa",2),("zebra",3)]
--}

verifyOccurrencesElement :: [(String, Int)] -> String -> Bool
verifyOccurrencesElement [] verify = False
verifyOccurrencesElement ((name, _):xs) verify
    | name == verify = True
    | otherwise = verifyOccurrencesElement xs verify

getLengthElement :: [String] -> String -> Int
getLengthElement [] verify = 0
getLengthElement (x:xs) verify
    | x == verify = 1 + getLengthElement xs verify
    | otherwise = getLengthElement xs verify

formatInTuples :: [String] -> [(String, Int)]
formatInTuples [] = []
formatInTuples (x:[]) = (x, 1):[]
formatInTuples (x:xs) = (x, 1):[] ++ formatInTuples xs

occurrences :: Eq a => [a] -> [(a, Int)]
occurrences [] = []
occurrences (x:xs) = (x, getAmountOccurrences) : occurrences (removeAllOccurrences)
    where removeAllOccurrences = filter (x /=) xs
          getAmountOccurrences = length (getEqualOccurrences) + 1
          getEqualOccurrences = filter (x ==) xs


{-
    5) Um grafo é uma estrutura de dados baseada em uma abstração matemática
    bastante popular. Grafos são usados em diferentes nichos de aplicação como
    implementação de mapas inteligentes, gerenciamento de tráfego em rede e
    interpretação de linguagens funcionais. Um grafo é um par consistindo em um
    conjunto de vértices e um conjunto de arestas. Cada aresta conecta um par de
    vértices. Diz-se que que dois vértices conectados por uma aresta são
    adjacentes. Em Haskell, é possível definir um grafo como um par:
    type Vertice = Int
    type Aresta = (Int, Int)
    type Grafo = ([Vertice], [Aresta])
    Nesta questão consideraremos grafos direcionados, ou seja, se um par (v1,
    v2) pertence à lista de arestas de um grafo, isso significa que há uma
    aresta de v1 para v2 mas não o contrário, ou seja, dizemos que v2 é um
    vizinho de v1 mas a recíproca não é verdadeira. Implemente uma função que
    recebe como entrada um grafo e produz como resultado uma lista de tuplas de
    dois elementos (V, VS), onde V é um nó do grafo VS é a lista de todos os nós
    vizinhos de V no grafo.
    Exemplo:
    Prelude> vizinhos ([1,2,3,4,5], [(1,2), (3,4), (1,4), (2, 1), (2,3), (2, 4),
    (5, 3), (4, 2)])
    [(1, [2, 4]), (3, [4]), (2, [1, 3, 4]), (5, [3]), (4, [2])]
--}

type Vertice = Int
type Aresta = (Int, Int)
type Grafo = ([Vertice], [Aresta])
