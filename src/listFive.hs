{--
    1) Crie o tipo Pergunta com os values constructors Sim ou Nao. Faça as funções
    seguintes, determinando seus tipos explicitamente.
--}



data Question = Yes | Not deriving Show


{-
    [A] recebe via parâmetro uma Pergunta . Retorna 0 para Nao e 1 para
    Sim.
--}
askNumber :: Question -> Int
askNumber Yes = 1
askNumber Not = 0

{-
    [B]: recebe via parâmetro uma lista de Perguntas , e retorna 0 s e 1 s
    correspondentes aos construtores contidos na lista.
--}
lisAsks :: [Question] -> [Int]
lisAsks [] = []
lisAsks (Yes:[]) = [1]
lisAsks (Not:[]) = [0]
lisAsks (x:xs) = askNumber x:[] ++ lisAsks(xs)


{-
    [C] : recebe duas Perguntas como parâmetro e retorna a tabela verdade do
    and lógico, usando Sim como verdadeiro e Nao como falso.
--}
andLogic :: Question -> Question -> Bool
andLogic Yes Yes = True
andLogic _ _ = False

{--
    [D] : idem ao anterior, porém deve ser usado o ou lógico.
--}
orLogic :: Question -> Question -> Bool
orLogic Not Not = False
orLogic _ _ = True

{--
    [E]: idem aos anteriores, porém usando o not lógico.
--}
notLogic :: Question -> Bool
notLogic Yes = False
notLogic Not = True

{--
    2) Faça o tipo Temperatura que pode ter valores Celsius, Farenheit ou Kelvin.
Implemente as funções:
--}
data Temp = Celsius | Farenheit | Kelvin deriving Show

convertTemp :: Double -> Temp -> Double
convertTemp number Celsius = (number * 9/5) + 32
convertTemp number Farenheit = 1.8 * number + 32   
convertTemp number Kelvin = number + 273

{--
    3) Implemente uma função que simule o vencedor de uma partida de pedra, papel e
tesoura usando tipos criados. Casos de empate devem ser considerados em seu
tipo.
--}
data OptionsGame = Rock | Paper | Scissor deriving (Show, Eq)
data ResultGame = Win | Lose | Draw deriving Show
game :: OptionsGame -> OptionsGame -> ResultGame
game Rock secondGame 
    | secondGame == Scissor = Win
    | secondGame == Paper = Lose

game Paper secondGame
    | secondGame == Rock = Win
    | secondGame == Scissor = Lose

game Scissor secondGame
    | secondGame == Paper = Win
    | secondGame == Rock = Lose

game firstGame secondGame   
    | firstGame == secondGame = Draw



{--
        4) Faça uma função que retorne uma string, com todas as vogais maiúsculas e
    minúsculas eliminadas de uma string passada por parâmetro usando list
    comprehension.
--}

isVowel :: Char -> Bool
isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' ||
    c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U'

deleteVowelsOfString :: String -> String
deleteVowelsOfString (x:xs) = [x | x <- (x:xs), not (isVowel x)]

{--
        5) Faça um novo tipo chamado Mes, que possui como valores todos os meses do ano.
    Implemente:
--}

data Month = January | February | March | April | May | June | July | August | September | October | November | December 
    deriving (Show)


{--
    a) A função checaFim , que retorna o número de dias que cada mês possui
        (considere fevereiro tendo 28 dias).

    b) A função prox, que recebe um mês atual e retorna o próximo mês.

    c) A função estação, que retorna a estação do ano de acordo com o mês e com o
        hemisfério.
--}

{--
    6) Faça uma função que receba uma String e retorne True se esta for um palíndromo;
    caso contrário, False.    
--}

palindrome :: String -> String 
palindrome (x:[]) = x
palindrome [] = ""
palindrome (x:xs) =  palindrome xs:[] ++ [x]


