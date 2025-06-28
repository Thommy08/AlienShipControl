import Data.List
import Data.Maybe
import Text.Read
import System.IO
import Control.Monad (foldM)

-- Definição do tipo Nave
type Nave = (String, ((Int, Int, Int), Bool), (Int, Int, Int), (Int, Int, Int))

-- Função para processar o texto em uma lista de naves
processarTexto :: String -> [Nave]
processarTexto input = map processarNave (lines input)
  where
    processarNave :: String -> Nave
    processarNave linha = 
      let partes = words linha
          idNave = head partes
          atributos = tail partes
      in case (findIndex (== "init") atributos, findIndex (== "initspace") atributos, findIndex (== "move") atributos) of
          (Just i, _, _) ->
            let posInicial = lerTupla $ atributos !! (i + 1)
                estadoInicial = if atributos !! (i + 2) == "1" then True else False
                estado = case findIndex (== "acao") atributos of
                           Just a -> if atributos !! (a + 1) == "(ligar)" then True else False
                           Nothing -> estadoInicial
                (espacoInicio, espacoFim) = 
                  case findIndex (== "initspace") atributos of
                    Just j -> (lerTupla $ atributos !! (j + 1), lerTupla $ atributos !! (j + 2))
                    Nothing -> ((0, 0, 0), (100, 100, 100))
                movimento = 
                  case findIndex (== "move") atributos of
                    Just k -> somarMovimentos posInicial (extrairMovimentos (drop (k + 1) atributos))
                    Nothing -> posInicial
            in if validarMovimento movimento espacoInicio espacoFim
               then (idNave, (movimento, estado), espacoInicio, espacoFim)
               else (idNave, ((0, 0, 0), estado), espacoInicio, espacoFim)  -- Corrige para posição inicial padrão
          (_, Just j, _) ->
            let (espacoInicio, espacoFim) = (lerTupla $ atributos !! (j + 1), lerTupla $ atributos !! (j + 2))
                estado = case findIndex (== "acao") atributos of
                           Just a -> if atributos !! (a + 1) == "(ligar)" then True else False
                           Nothing -> False
                movimento = 
                  case findIndex (== "move") atributos of
                    Just k -> somarMovimentos (0, 0, 0) (extrairMovimentos (drop (k + 1) atributos))
                    Nothing -> (0, 0, 0)
            in if validarMovimento movimento espacoInicio espacoFim
               then (idNave, (movimento, estado), espacoInicio, espacoFim)
               else (idNave, ((0, 0, 0), estado), espacoInicio, espacoFim)
          (_, _, Just k) ->
            let movimento = somarMovimentos (0, 0, 0) (extrairMovimentos (drop (k + 1) atributos))
            in if validarMovimento movimento (0, 0, 0) (100, 100, 100)
               then (idNave, (movimento, False), (0, 0, 0), (100, 100, 100))
               else (idNave, ((0, 0, 0), False), (0, 0, 0), (100, 100, 100))
          _ -> error $ "Formato inválido de linha: " ++ linha

-- Função para ler uma tupla no formato (x,y,z)
lerTupla :: String -> (Int, Int, Int)
lerTupla s =
  let sLimpo = filter (`notElem` "()") s
      valores = mapMaybe readMaybe (split ',' sLimpo)
  in case valores of
       [x, y, z] -> (x, y, z)
       _ -> error "Formato de tupla inválido"

-- Função para dividir uma string com base em um delimitador
split :: Char -> String -> [String]
split _ [] = []
split delim s = let (parte1, resto) = break (== delim) s
                in parte1 : case resto of
                              [] -> []
                              (_:xs) -> split delim xs

-- Função para somar movimentos
somarMovimentos :: (Int, Int, Int) -> [(Int, Int, Int)] -> (Int, Int, Int)
somarMovimentos (x, y, z) movimentos = foldl (\(sx, sy, sz) (mx, my, mz) -> (sx + mx, sy + my, sz + mz)) (x, y, z) movimentos

-- Função para extrair movimentos após a palavra "move"
extrairMovimentos :: [String] -> [(Int, Int, Int)]
extrairMovimentos [] = []
extrairMovimentos (x:xs) = 
  if head x == '(' then lerTupla x : extrairMovimentos xs else []

-- Verifica se o movimento está dentro dos limites
validarMovimento :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Bool
validarMovimento (x, y, z) (x1, y1, z1) (x2, y2, z2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2

-- Atualiza o estado da nave, garantindo que o movimento seja válido
atualizarEstado :: Nave -> (Int, Int, Int) -> Nave
atualizarEstado (idNave, (_, ligado), espacoInicio, espacoFim) novoMovimento =
  if validarMovimento novoMovimento espacoInicio espacoFim
    then (idNave, (novoMovimento, ligado), espacoInicio, espacoFim)
    else (idNave, ((0, 0, 0), ligado), espacoInicio, espacoFim) -- Movimenta para posição padrão ou mantém a anterior

-- Executa ações na nave, validando movimentos
executarAcoes :: Nave -> [Nave] -> IO [Nave]
executarAcoes nave naves = do
  let (idNave, (posAtual, ligado), espacoInicio, espacoFim) = nave
  if not ligado
    then do
      putStrLn (idNave ++ ": Nave está desligada. Ligue-a primeiro.")
      return naves
    else do
      let novoMovimento = posAtual
      putStrLn $ idNave ++ ": Posição atual em " ++ show novoMovimento
      return naves

-- Altera o estado de uma nave (ligar/desligar)
alterarEstado :: String -> Bool -> [Nave] -> IO [Nave]
alterarEstado idNave novoEstado naves = do
  let navesAtualizadas = map (\n@(id, (pos, ligado), espMin, espMax) -> 
                                if id == idNave then (id, (pos, novoEstado), espMin, espMax) else n) naves
  putStrLn $ idNave ++ ": Estado alterado para " ++ if novoEstado then "Ligado" else "Desligado"
  return navesAtualizadas

-- Inicializa a nave com uma posição e estado
inicializarNave :: String -> (Int, Int, Int) -> Bool -> [Nave] -> IO [Nave]
inicializarNave idNave novaPosicao novoEstado naves = do
  let navesAtualizadas = map (\n@(id, _, espMin, espMax) ->
                                if id == idNave
                                then (id, (novaPosicao, novoEstado), espMin, espMax)
                                else n) naves
  putStrLn $ idNave ++ ": Nave inicializada na posição " ++ show novaPosicao
  return navesAtualizadas

-- Define o espaço de movimentação de uma nave
definirEspaco :: String -> ((Int, Int, Int), (Int, Int, Int)) -> [Nave] -> IO [Nave]
definirEspaco idNave limites naves = do
  let navesAtualizadas = map (\n@(id, estado, _, _) ->
                                if id == idNave
                                then (id, estado, fst limites, snd limites)
                                else n) naves
  putStrLn $ idNave ++ ": Espaço definido com limites " ++ show limites
  return navesAtualizadas

-- Função auxiliar para acessar o ID da nave
fst4 :: Nave -> String
fst4 (idNave, _, _, _) = idNave

-- Menu interativo
menu :: [Nave] -> IO ()
menu naves = do
  putStrLn "Menu de Operações:"
  putStrLn "1. Listar todas as naves"
  putStrLn "2. Executar ações de uma nave"
  putStrLn "3. Executar ações de todas as naves"
  putStrLn "4. Inserir ação manualmente"
  putStrLn "5. Sair"
  putStrLn "Escolha uma opção:"
  opcao <- getLine
  case opcao of
    "1" -> do
      mapM_ print naves
      menu naves
    "2" -> do
      putStrLn "Digite o ID da nave:"
      idNave <- getLine
      case find ((== idNave) . fst4) naves of
        Just nave -> executarAcoes nave naves >>= menu
        Nothing -> putStrLn "Nave não encontrada." >> menu naves
    "3" -> do
      novasNaves <- foldM (\ns nave -> executarAcoes nave ns) naves naves
      menu novasNaves
    "4" -> do
      putStrLn "Digite a ação manualmente (Exemplo: oi98 acao (ligar), oi98 move (10,20,30), oi98 init (5,5,5) 1, oi98 initspace (0,0,0) (100,100,100)):"
      acao <- getLine
      let palavras = words acao
      case palavras of
        (idNave:"acao":"(ligar)":_) -> do
          novasNaves <- alterarEstado idNave True naves
          menu novasNaves
        (idNave:"acao":"(desligar)":_) -> do
          novasNaves <- alterarEstado idNave False naves
          menu novasNaves
        (idNave:"move":movimentos) -> do
          let novosMovimentos = map lerTupla movimentos
          novasNaves <- executarMovimento idNave novosMovimentos naves
          menu novasNaves
        (idNave:"init":pos:estado:_) -> do
          let novaPosicao = lerTupla pos
              novoEstado = estado == "1"
          novasNaves <- inicializarNave idNave novaPosicao novoEstado naves
          menu novasNaves
        (idNave:"initspace":limMin:limMax:_) -> do
          let limites = (lerTupla limMin, lerTupla limMax)
          novasNaves <- definirEspaco idNave limites naves
          menu novasNaves
        _ -> do
          putStrLn "Ação inválida. Tente novamente."
          menu naves
    "5" -> putStrLn "Saindo..."
    _   -> putStrLn "Opção inválida." >> menu naves

-- Executa movimentos manualmente
executarMovimento :: String -> [(Int, Int, Int)] -> [Nave] -> IO [Nave]
executarMovimento idNave movimentos naves = do
  case find ((== idNave) . fst4) naves of
    Just (idNave, (posAtual, ligado), espacoInicio, espacoFim) -> do
      let novoMovimento = somarMovimentos posAtual movimentos
      if validarMovimento novoMovimento espacoInicio espacoFim
        then do
          let naveAtualizada = (idNave, (novoMovimento, ligado), espacoInicio, espacoFim)
          putStrLn $ idNave ++ ": Movimento realizado com sucesso para " ++ show novoMovimento
          return $ map (\n -> if fst4 n == idNave then naveAtualizada else n) naves
        else do
          putStrLn $ idNave ++ ": Movimento inválido! Fora dos limites permitidos."
          return naves
    Nothing -> do
      putStrLn "Nave não encontrada."
      return naves

-- Função principal
main :: IO ()
main = do
  conteudo <- readFile "Alienship.txt"
  let naves = processarTexto conteudo
  menu naves
