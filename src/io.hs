import System.IO

escreverArq :: IO ()
escreverArq = do
  arquivo <- openFile "exemplo01.txt" WriteMode
  hPutStr arquivo "Aula de programacao funcional\n"
  hPutStrLn arquivo "Boa noite pessoal!"
  putStrLn "Escrita em arquivo bem sucedida"
  hFlush arquivo
  hClose arquivo


adicionarDados :: IO ()
adicionarDados = do
	arquivo <- openFile "exemplo01.txt" AppendMode
	hPutStr arquivo "\nAdicionado dados em um arquivo existente.\n"
	hPutStrLn arquivo "Hoje a aula eh sobre arquivos em haskell!"
	putStrLn "Alteracao bem sucedida"
	hFlush arquivo
	hClose arquivo

lerArquivo :: IO ()
lerArquivo = do
	arquivo <- openFile "exemplo01.txt" ReadMode
	dados <- hGetContents arquivo
	putStrLn dados
	hClose arquivo

escreverDireto :: IO ()
escreverDireto = do
	writeFile "exemplo02.txt" "Manipulando arquivos em haskell\n"
	putStrLn "Operacao realizada com sucesso!"

lerDireto :: IO ()
lerDireto = do
	texto <- readFile "exemplo01.txt"
	texto2 <- readFile "exemplo02.txt"
	putStrLn "texto do primeiro arquivo"
	putStrLn texto
	putStrLn "texto do segundo arquivo"
	putStrLn texto2

adicionarDadosDireto :: IO ()
adicionarDadosDireto = do
	appendFile "exemplo02.txt" "Viu com eh facil"
	putStrLn "Operacao realizada com sucesso!"