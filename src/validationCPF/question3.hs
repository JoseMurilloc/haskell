{-- QUESTÃO 3: Cadastro de Alunos
Crie um Sistema de Cadastro de Alunos. Os dados a serem cadastrados são: 
Nome, CPF e nota. 
  [] Lembre-se de validar o CPF antes de cadastrar. 
  [] O sistema deve gerar um relatório em um arquivo .txt, 
  [] contendo os dados de todos os alunos, a maior nota, a menor nota e a média da turma. 
  [x] O sistema deve conter um menu com as operações abaixo: --}
  -- 1. Cadastrar Aluno
  -- 2. Gerar Relatório
  -- 3. Busca por nome ok
  -- 4. Busca por CPF ok
  -- 5. Média da Turma ok
  -- 6. Excluir Cadastro
  -- 7. Sair do Sistema

import System.IO
import Data.List

type Name = String
type Score = Float
type CPF = String

type Student = (CPF, Name, Score)

type ClassRoom = [Student]

classRoomDB :: ClassRoom
classRoomDB = [
  ("111.444.777-05", "Fulano1", 7.0),
  ("111.444.777-02", "Fulano2", 7.0),
  ("111.444.777-03", "Fulano3", 7.5),
  ("111.444.777-01", "Fulano4", 8),
  ("111.444.777-12", "Fulano5", 8.5),
  ("111.444.777-31", "Fulano6", 10)]


mediaClassRoom :: ClassRoom -> Float
mediaClassRoom db = (foldl (\a (_,_,s) -> s+a) 0.0 db) / (sizeClassRoom db)

getBiggerScore :: ClassRoom -> Float
getBiggerScore [] = 0
getBiggerScore ((_,_,score):[]) = score
getBiggerScore ((_,_,score):xs)
  | score > getBiggerScore xs = score
  | otherwise = getBiggerScore xs

getSmallerScore :: ClassRoom -> Float
getSmallerScore [] = 0
getSmallerScore ((_,_,score):[]) = score
getSmallerScore ((_,_,score):xs)
  | score < getSmallerScore xs = score
  | otherwise = getSmallerScore xs

sizeClassRoom :: ClassRoom -> Float
sizeClassRoom [] = 0.0
sizeClassRoom (x:xs) = 1.0 + sizeClassRoom xs  

searchByName :: Name -> ClassRoom -> Student
searchByName name [] = ("no-cpf", "no-student", 0.0)
searchByName name ((cpf, n, score):as) 
  | name == n = (cpf, n, score)
  | otherwise = searchByName name as

searchByCPF :: CPF -> ClassRoom -> Student
searchByCPF cpf [] = ("no-cpf", "no-student", 0.0)
searchByCPF cpf ((c, n, score):as) 
  | cpf == c = (c, n, score)
  | otherwise = searchByCPF cpf as


deleteStudent :: CPF -> ClassRoom -> ClassRoom
deleteStudent cpf db = [(c,n,s) | (c,n,s) <- db, cpf /= c]


registerStudent :: Student -> ClassRoom -> ClassRoom
registerStudent student db = db ++ [student]



writeValidationCPFResult :: String -> IO()
writeValidationCPFResult students = do
    arquivo <- openFile "relatorio.txt" AppendMode

    hPutStrLn arquivo students

    hFlush arquivo
    hClose arquivo


resetContentFile :: IO()
resetContentFile = do
    writeFile "relatorio.txt" ""


listStudent :: ClassRoom -> String
listStudent db = (foldl (\a (cpf,name,score) -> a++"CPF: "++cpf++" Aluno: "++name++"\n") "" db)


start :: IO()
start = do
  putStrLn "=========================== FACAPE ================================"
  putStrLn "| Gravar relatorio"
  let students = listStudent classRoomDB
  resetContentFile
  
  
  let media = mediaClassRoom classRoomDB 
  let mediaRelatorio = show media 
  let mediaRelatotioText = ("\n\n====== Relatorio de notas ======== \n\nMedia: " ++ mediaRelatorio)
  
  let biggerScore = getBiggerScore classRoomDB
  let biggerScoreRelatorio = ("\nMaior nota: " ++ (show biggerScore)) 

  let smallerScore = getSmallerScore classRoomDB
  let smallerScoreRelatorio = ("\nMenor nota: " ++ (show smallerScore)) 
  
  let strRelatorio = students ++ mediaRelatotioText ++ biggerScoreRelatorio ++ smallerScoreRelatorio
  writeValidationCPFResult strRelatorio

  putStr "Pesquisa por nome: "
  searchInput <- getLine
  let studentSearchByName = searchByName searchInput classRoomDB   
  putStrLn $ show studentSearchByName






main :: IO()
main = do 
  start
  putStrLn "=========================== FACAPE ================================"