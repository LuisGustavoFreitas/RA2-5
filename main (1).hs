import Types
import Logic
import Reports

import qualified Data.Map as M
import System.IO (withFile, IOMode(ReadMode), hGetContents, hFlush, stdout)
import Control.Exception (catch, IOException)
import Data.Time.Clock
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)

invFile, logFile :: FilePath
invFile = "Inventario.dat"
logFile = "Auditoria.log"

main :: IO ()
main = do
  putStrLn " "
  putStrLn "        Sistema de Inventário"
  putStrLn " "
  putStrLn " Digite 'help' para ver a lista de comandos."
  putStrLn " "
  inv <- loadInventario
  logs <- loadLogs
  loop inv logs

loadInventario :: IO Inventario
loadInventario = do
  exists <- doesFileExist invFile
  if not exists
    then do
      putStrLn "Inventário não encontrado, criando novo."
      return M.empty
    else do
      contents <- readFile invFile
      length contents `seq` return ()   
      case readMaybe contents of
        Just inv -> return inv
        Nothing  -> do
          putStrLn "Erro ao ler inventário, iniciando vazio."
          return M.empty

loadLogs :: IO [LogEntry]
loadLogs = do
  exists <- doesFileExist logFile
  if not exists
    then return []
    else do
      contents <- readFile logFile
      length contents `seq` return ()
      return [l | Just l <- map readMaybe (lines contents)]

persistInventario :: Inventario -> IO ()
persistInventario inv = writeFile invFile (show inv)

appendLog :: LogEntry -> IO ()
appendLog le = appendFile logFile (show le ++ "\n")

loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
  putStr "\n> "
  hFlush stdout
  line <- getLine
  now <- getCurrentTime
  case words line of
    ["help"] -> putStrLn helpText >> loop inv logs
    ["list"] -> do putStrLn (listItemsView inv)
                   loop inv logs
    ["exit"] -> putStrLn "Saindo..." >> exitSuccess


    ("add":iid:nome:q:cat) ->
      case readMaybe q of
        Nothing -> do
            putStrLn "Quantidade inválida."
            failLog now (Add iid) "Quantidade inválida"
            loop inv logs
        Just qtd ->
          let item = Item iid nome qtd (unwords cat) in
          case addItem now item inv of
            Left e  -> putStrLn e >> failLog now (Add iid) e >> loop inv logs
            
            Right (newInv, logE) ->
                persistInventario newInv >> appendLog logE >>
                putStrLn "Item adicionado." >> loop newInv (logs++[logE])

    
    ("add":_) -> do
        putStrLn "Uso correto: add <id> <nome> <quantidade> <categoria>"
        failLog now (QueryFail line) "Formato inválido de add"
        loop inv logs


    ("remove":iid:q:_) ->
      case readMaybe q of
        Nothing -> failLog now (Remove iid 0) "quantidade inválida" >> loop inv logs
        Just qtd ->
          case removeItem now iid qtd inv of
            Left e  -> do
                putStrLn e
                failLog now (Remove iid qtd) e
                loop inv logs

            Right (newInv, logE) ->
              persistInventario newInv >> appendLog logE >>
              putStrLn "Removido." >> loop newInv (logs++[logE])

    ("update":iid:q:_) ->
      case readMaybe q of
        Nothing -> failLog now (Update iid 0) "quantidade inválida" >> loop inv logs
        Just qtd ->
          case updateQty now iid qtd inv of
            Left e  -> do 
                putStrLn e
                failLog now (Update iid qtd) e 
                loop inv logs
            
            Right (newInv, logE) ->
              persistInventario newInv >> appendLog logE >>
              putStrLn "Atualizado." >> loop newInv (logs++[logE])
              
    ("delete":iid:_) -> do
      case deleteItem now iid inv of
        Left e -> do
            putStrLn e
            failLog now (Delete iid) e
            loop inv logs

        Right (newInv, logE) -> do
            persistInventario newInv
            appendLog logE
            putStrLn $ "Item " ++ iid ++ " removido por completo."
            loop newInv (logs++[logE])


    ["report"] -> do
        freshLogs <- loadLogs
        putStrLn "Erros:"
        mapM_ print (logsDeErro freshLogs)
        putStrLn "Itens mais movimentados:"
        mapM_ print (take 5 $ itemMaisMovimentado freshLogs)
        loop inv freshLogs


    _ -> failLog now (QueryFail line) "comando inválido" >> putStrLn "Comando inválido." >> loop inv logs

failLog :: UTCTime -> AcaoLog -> String -> IO ()
failLog now a msg = appendLog (LogEntry now a msg (Falha msg))

helpText :: String
helpText = unlines
  [ " "
  , "       Lista de Comandos do Sistema de Inventário"
  , ""
  , " add <id> <nome> <quantidade> <categoria>"
  , "      Adiciona um novo item ao inventário."
  , "      Exemplo: add I001 Mouse 10 Perifericos"
  , ""
  , " remove <id> <quantidade>"
  , "      Remove uma quantidade específica de um item."
  , "      Exemplo: remove I001 2"
  , ""
  , " update <id> <nova_quantidade>"
  , "      Define uma nova quantidade total para o item."
  , "      Exemplo: update I001 15"
  , ""
  , " delete <id>"
  , "      Deleta um item específico."
  , "      Ex: delete I001"
  , ""
  , " list"
  , "      Mostra todos os itens atualmente no inventário."
  , ""
  , " report"
  , "      Exibe o relatório de erros e os itens mais movimentados."
  , ""
  , " help"
  , "      Mostra esta lista de comandos e suas explicações."
  , ""
  , " exit"
  , "      Encerra o programa."
  , ""
  ]
