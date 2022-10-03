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


parseFromReal :: Int -> String
parseFromReal value
    | value >= 10 = show (div value 100) ++ "." ++ show ( mod value 100)
    | otherwise = "0.0" ++ show (mod value 100) 


generatorPointersGap :: Name -> Price -> String
generatorPointersGap name price = replicate (sizeLine - (length name + length (parseFromReal price))) '.'

formatLine :: (Name, Price) -> String;
formatLine (name, price) = name ++ (generatorPointersGap name price) ++ parseFromReal price ++ "\n"

formatLines :: [(Name, Price)] -> String
formatLines (x:[]) = formatLine x
formatLines (x:xs) = formatLine x ++ formatLines xs


formatTotal :: Price -> String
formatTotal total = formatLine ("Total", total)


type Accounts = (Name, Price)
formatAccount :: Accounts -> String
formatAccount listProducts  = "Haskell store\n\n" ++ formatLines listProducts ++ formatTotal (foldl sumTotal 0 listProducts)
    where sumTotal (name, price) acc = price + acc 









