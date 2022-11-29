
import System.IO
import Data.List

type Name = String
type Score = Float
type CPF = String

type Student = (CPF, Name, Score)

type ClassRoom = [Student]


classRoomDB = []

menu :: IO ()
menu = do
      putStrLn . unlines $ map concatNums choices
      choice <- getLine
      case validate choice of
         Just n  -> execute . read $ choice
         Nothing -> putStrLn "Please try again"

      menu
   where concatNums (i, (s, _)) = show i ++ ".) " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, IO ()))]
choices = zip [1.. ] [
   ("Cadastrar aluno", register),
   ("Gera relatorio", foo),
   ("Busca por nome", search),
   ("Busca por CPF", foo),
   ("Media da Turma", foo),
   ("Excluir cadastro", foo),
   ("Sair do Sistema", bar)
 ]

execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f

foo = hello
register = registerUser 
search = searchName
bar = undefined

hello = do
    putStrLn "hello"

registerUser = do
    cpf <- getLine
    name <- getLine
    score <- getLine
    let scoreFloat = read score :: Float
    registerStudent (cpf, name, scoreFloat)
    putStrLn "Aluno cadastrado :D"

searchName = do
    name <- getLine
    let classRoomDB = searchByName name
    putStrLn "Aluno cadastrado :D"
    

-- mediaClassRoom :: ClassRoom -> Float
-- mediaClassRoom db = (foldl (\a (_,_,s) -> s+a) 0.0 db) / (sizeClassRoom db)

-- sizeClassRoom :: ClassRoom -> Float
-- sizeClassRoom [] = 0.0
-- sizeClassRoom (x:xs) = 1.0 + sizeClassRoom xs  

searchByName :: Name -> ClassRoom -> Student
searchByName name [] = ("no-cpf", "no-student", 0.0)
searchByName name ((cpf, n, score):as) 
  | name == n = (cpf, n, score)
  | otherwise = searchByName name as

-- searchByCPF :: CPF -> ClassRoom -> Student
-- searchByCPF cpf [] = ("no-cpf", "no-student", 0.0)
-- searchByCPF cpf ((c, n, score):as) 
--   | cpf == c = (c, n, score)
--   | otherwise = searchByCPF cpf as


-- deleteStudent :: CPF -> ClassRoom -> ClassRoom
-- deleteStudent cpf db = [(c,n,s) | (c,n,s) <- db, cpf /= c]


registerStudent :: Student -> IO()
registerStudent student = do
    let a = classRoomDB ++ [student]
    let classRoomDB = a
    putStrLn $ show classRoomDB
    putStrLn "Registrado"