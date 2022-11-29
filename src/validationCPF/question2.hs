-- QUESTÃO 2: Validação de CNPJ
-- Resolva o mesmo problema da questão anterior, só que para validar CNPJ.
-- Relatório de Validação de CNPJ

-- baseCNPJ: https://www.macoratti.net/alg_cnpj.htm#:~:text=Algoritmo%20para%20valida%C3%A7%C3%A3o%20do%20CNPJ&text=O%20n%C3%BAmero%20que%20comp%C3%B5e%20o,que%20s%C3%A3o%20os%20d%C3%ADgitos%20verificadores.

import System.IO
import Data.Char
import Control.Monad (replicateM)

sizeLine :: Int
sizeLine = 45

extractCNPJ :: String -> (String, String)
extractCNPJ cpnj = do
  let [cpnjNumber, verifyCode] = wordsWhen (=='-') cpnj
  (cpnjNumber, verifyCode)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen condition s =  case dropWhile condition s of
                  "" -> []
                  s' -> w : wordsWhen condition s''
                        where (w, s'') = break condition s' 


covertCNPJToInt :: String -> [Int]
covertCNPJToInt cpnj = [digitToInt x | x <- cpnj]

validationCNPJ :: [Int] -> Int -> [Int]
validationCNPJ (x:[]) index = [x * index]
validationCNPJ (x:xs) index = (x * index):[] ++ validationCNPJ xs controlIndex
  where controlIndex | index == 2 = 9 | otherwise = (index-1)

generatorCodeValidation :: [Int] -> Int
generatorCodeValidation cpnj
  | (mod totalCNPJ 11) < 2 = 0
  | otherwise = 11 - restCNPJ 
    where totalCNPJ = (sum cpnj)
          restCNPJ = mod totalCNPJ 11


writeValidationCPFResult :: String -> IO()
writeValidationCPFResult cnpjLines = do
    arquivo <- openFile "cnpjsValidado.txt" AppendMode

    hPutStrLn arquivo cnpjLines
    putStrLn "Alteracao bem sucedida"

    hFlush arquivo
    hClose arquivo


resetContentFile :: IO()
resetContentFile = do
    writeFile "cnpjsValidado.txt" ""
    putStrLn "Reset bem sucedida"

generatorPointersGap :: String -> String -> String
generatorPointersGap name isValid = replicate (sizeLine - (length name + length isValid)) '.'

validatedCNPJ :: String -> String
validatedCNPJ inputCNPJ = do
    let (cpnj, verifyCode) = extractCNPJ inputCNPJ
    let removedPointersCNPJ = [x | x <- cpnj, x /= '.', x /= '/']
    let cpnjNumberInt = covertCNPJToInt removedPointersCNPJ 
    let arrayInt = validationCNPJ cpnjNumberInt 5
    let firstDigit = generatorCodeValidation arrayInt

    let arrayInt2  = validationCNPJ (cpnjNumberInt ++ [firstDigit]) 6
    let secondDigit = generatorCodeValidation arrayInt2
    let parseString1 = show (firstDigit)
    let parseString2 = show (secondDigit)
    let userCNPJValidate = cpnj ++ "-" ++ parseString1 ++ parseString2 :: String
    
    if inputCNPJ == userCNPJValidate then
      inputCNPJ ++ generatorPointersGap inputCNPJ "valido" ++ "valido\n"
    else
      inputCNPJ ++ generatorPointersGap inputCNPJ "invalido" ++ "invalido\n"


main :: IO()
main = do 
    fileCNPJ <- openFile "cnpjs.txt" ReadMode
    content <- hGetContents fileCNPJ
    let linesOfFiles = lines content

    let cnpjLinesValidated = [validatedCNPJ(x) | x <- linesOfFiles]
    resetContentFile

    let cnpjLines = foldl (\x a -> a++x) "" cnpjLinesValidated :: String
    writeValidationCPFResult cnpjLines

    hClose fileCNPJ 
    putStrLn "=================================="
