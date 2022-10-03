

{--
    1) Cupom fiscal do supermercado
Nas tarefas que se seguem temos por objetivo desenvolver uma aplicação em Haskell para
automatizar o caixa de um supermercado usando técnicas de manipulação de listas
empregando funções de ordem superior. Um leitor de código de barras é usado no caixa de
um supermercado para produzir uma lista de códigos de barras a partir dos produtos que se
encontram em um carrinho de compras contendo os produtos comprados. Usando os códigos
de barra cria-se uma nota descritiva da compra. Considere por exemplo a seguinte lista de
códigos de barra:
--}


type Name = String
type Price = Int
type Code = Int


type Cart = [Code]
type Account = [(Name, Price)]

type Merchandise = [(Code, Name, Price)]

sizeLine :: Int
sizeLine = 30

tableMerchandise :: Merchandise
tableMerchandise = [
    (5235, "First Fingers", 121),
    (5632, "Nappies", 1010),
    (4545, "Orange Jelly", 56),
    (1111, "Hula Hoops", 21),
    (1112, "Hula Hoops (Giant)", 133),
    (1234, "Dry SHERRY 2LT", 540)]


-- incrementZero :: Int -> String
-- incrementZero number
--     | number > 10  = "0" ++ number
--     | otherwise = number

parseFromReal :: Int -> String
parseFromReal value
    | value >= 10 = show (div value 100) ++ "." ++ show ( mod value 100)
    | otherwise = "0.0" ++ show (mod value 100) 


generatorGap :: Name -> Price -> String
generatorGap name price = replicate (sizeLine - (length name + length (parseFromReal price))) '.'


formatLine :: (Name, Price) -> String;
formatLine (name, price) = name ++ (generatorGap name price) ++ parseFromReal price

formatLines :: [(Name, Price)] -> String
formatLines (x:[]) = formatLine x
formatLines (x:xs) = formatLine x ++ formatLines xs

formatTotal :: Price -> String
formatTotal price = "\nTotal" ++ (generatorGap "Total" price) ++ parseFromReal price