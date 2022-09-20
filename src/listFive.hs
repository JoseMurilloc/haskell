-- Crie o tipo Pergunta com os values constructors Sim ou Nao. Faça as funções
-- seguintes, determinando seus tipos explicitamente.

data Question = Yes | Not deriving Show
askNumber :: Question -> Int
askNumber Yes = 1
askNumber Not = 0

askNumberList :: [Question] -> [Int]
askNumberList [] = []
askNumberList (Yes:[]) = [1]
askNumberList (Not:[]) = [0]
askNumberList (x:xs) = askNumber x:[] ++ askNumberList(xs)


andLogic :: Question -> Question -> Bool
andLogic Yes Yes = True
andLogic _ _ = False

orLogic :: Question -> Question -> Bool
orLogic Not Not = False
orLogic _ _ = True

notLogic :: Question -> Bool
notLogic Yes = False
notLogic Not = True

-- 2
data Temp = Celsius | Farenheit | Kelvin deriving Show

convertTemp :: Double -> Temp -> Double
convertTemp number Celsius = (number * 9/5) + 32
convertTemp number Farenheit = 1.8*number+32   
convertTemp number Kelvin = number +273

-- 3