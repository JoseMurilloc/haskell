-- CPF validation
import System.IO

-- let cpf = "086.845.874-05"

-- ([Int], (Int, Int))

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen condition s =  case dropWhile condition s of
                  "" -> []
                  s' -> w : wordsWhen condition s''
                        where (w, s'') = break condition s'

separatorVerifyCode :: String -> String
separatorVerifyCode cpf = extractPointer
  where extractPointer = [x | x <- cpf, x /= '.']

initial :: [String]
initial = wordsWhen (=='-') (separatorVerifyCode "895.456.456-56")

-- mappingCPF :: String -> Int -> [Int]
-- mappingCPF cpf index = [digitToInt (x) * (succ index) | x <- cpf]

sumOne x = x-1; 

-- handle :: [Int] -> [Int]
-- handle (x:[]) = [x]
-- handle (x:xs) = 
