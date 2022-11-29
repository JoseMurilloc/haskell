import System.IO
import Data.Char
import Control.Monad (replicateM)
import Data.List

sizeLine :: Int
sizeLine = 45


-- baseCPF: https://www.macoratti.net/alg_cpf.htm#:~:text=O%20algoritmo%20de%20valida%C3%A7%C3%A3o%20do,%3A%20111.444.777%2D05.


extractCPF :: String -> (String, String)
extractCPF cpf = do
  let [cpfNumber, verifyCode] = wordsWhen (=='-') cpf
  (cpfNumber, verifyCode)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen condition s =  case dropWhile condition s of
                  "" -> []
                  s' -> w : wordsWhen condition s''
                        where (w, s'') = break condition s' 


covertCPFToInt :: String -> [Int]
covertCPFToInt cpf = [digitToInt x | x <- cpf]

validationCPF :: [Int] -> Int -> [Int]
validationCPF (x:[]) index = [x * index]
validationCPF (x:xs) index = (x * index):[] ++ validationCPF xs (index-1)

generatorCodeValidation :: [Int] -> Int
generatorCodeValidation cpf
  | (mod totalCPF 11) < 2 = 0
  | otherwise = 11 - restCPF 
    where totalCPF = (sum cpf)
          restCPF = mod totalCPF 11

generatorPointersGap :: String -> String -> String
generatorPointersGap name isValid = replicate (sizeLine - (length name + length isValid)) '.'


validatedCPF :: String -> String
validatedCPF inputCPF = do
    let (cpf, verifyCode) = extractCPF inputCPF
    let removedPointersCPF = [x | x <- cpf, x /= '.']
    let cpfNumberInt = covertCPFToInt removedPointersCPF 
    let arrayInt = validationCPF cpfNumberInt 10
    let firstDigit = generatorCodeValidation arrayInt

    let arrayInt2  = validationCPF (cpfNumberInt ++ [firstDigit]) 11
    let secondDigit = generatorCodeValidation arrayInt2
    let parseString1 = show (firstDigit)
    let parseString2 = show (secondDigit)
    let userCPFValidate = cpf ++ "-" ++ parseString1 ++ parseString2 :: String
    if inputCPF == userCPFValidate then
      inputCPF ++ generatorPointersGap inputCPF "valido" ++ "valido\n"
    else
      inputCPF ++ generatorPointersGap inputCPF "invalido" ++ "invalido\n"


writeValidationCPFResult :: String -> IO()
writeValidationCPFResult cpfLines = do
    arquivo <- openFile "cpfsValidado.txt" AppendMode

    hPutStrLn arquivo cpfLines
    putStrLn "Alteracao bem sucedida"

    hFlush arquivo
    hClose arquivo


resetContentFile :: IO()
resetContentFile = do
    writeFile "cpfsValidado.txt" ""
    putStrLn "Reset bem sucedida"

main :: IO()
main = do 
    fileCPF <- openFile "cpfs.txt" ReadMode
    content <- hGetContents fileCPF
    let linesOfFiles = lines content

    let cpfLinesValidated = [validatedCPF(x) | x <- linesOfFiles]
    resetContentFile

    let cpfLines = foldl (\x a -> a++x) "" cpfLinesValidated :: String
    writeValidationCPFResult cpfLines

    hClose fileCPF 
    putStrLn "=================================="