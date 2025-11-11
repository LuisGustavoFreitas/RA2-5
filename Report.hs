module Reports (logsDeErro, historicoPorItem, itemMaisMovimentado) where

import Types
import qualified Data.List as L
import qualified Data.Map as M

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\l -> case status l of Falha _ -> True; _ -> False)

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem iid = filter match
  where
    match le = case acao le of
      Add x       -> x == iid
      Remove x _  -> x == iid
      Update x _  -> x == iid
      QueryFail s -> iid `L.isInfixOf` s
      _           -> False


itemMaisMovimentado :: [LogEntry] -> [(String, Int)]
itemMaisMovimentado logs =
  let ids = concatMap getID logs
      freq = M.toList $ foldr (\k -> M.insertWith (+) k 1) M.empty ids
  in L.sortBy (\(_,a) (_,b) -> compare b a) freq
  where
    getID le = case acao le of
      Add x       -> [x]
      Remove x _  -> [x]
      Update x _  -> [x]
      _           -> []
