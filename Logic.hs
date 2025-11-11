module Logic (addItem, removeItem, updateQty, deleteItem,  listItemsView) where

import Types
import qualified Data.Map as M
import Data.Time.Clock (UTCTime)

addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem now it inv =
  if M.member (itemID it) inv
    then Left $ "Item com ID " ++ itemID it ++ " já existe."
    else
      let newInv = M.insert (itemID it) it inv
          logE = LogEntry now (Add (itemID it)) (show it) Sucesso
      in Right (newInv, logE)

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem now iid q inv =
  case M.lookup iid inv of
    Nothing -> Left $ "Item " ++ iid ++ " não encontrado."
    Just it ->
      let cur = quantidade it in
      if q <= 0
        then Left "Quantidade deve ser > 0."
      else if q > cur
        then Left $ "Estoque insuficiente: tem " ++ show cur ++ " unidades."
      else
        let newItem = it { quantidade = cur - q }
            newInv = if quantidade newItem <= 0
                     then M.delete iid inv
                     else M.insert iid newItem inv
            logE = LogEntry now (Remove iid q)
                     ("removido " ++ show q ++ " de " ++ iid) Sucesso
        in Right (newInv, logE)

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty now iid newQ inv
  | newQ < 0 = Left "Quantidade negativa inválida."
  | otherwise =
      case M.lookup iid inv of
        Nothing -> Left $ "Item " ++ iid ++ " não encontrado."
        Just it ->
          let newItem = it { quantidade = newQ }
              newInv = M.insert iid newItem inv
              logE = LogEntry now (Update iid newQ)
                         ("nova qtd " ++ show newQ ++ " para " ++ iid) Sucesso
          in Right (newInv, logE)
          
deleteItem :: UTCTime -> String -> Inventario -> Either String ResultadoOperacao
deleteItem now iid inv =
  case M.lookup iid inv of
    Nothing -> Left $ "Item " ++ iid ++ " não encontrado."
    Just _  ->
      let newInv = M.delete iid inv
          logE = LogEntry now (Delete iid) ("Item " ++ iid ++ " removido completamente") Sucesso
      in Right (newInv, logE)


listItemsView :: Inventario -> String
listItemsView inv =
  unlines $ ["ID | Nome | Quantidade | Categoria"]
          ++ map f (M.elems inv)
  where
    f it = itemID it ++ " | " ++ nome it ++ " | "
         ++ show (quantidade it) ++ " | " ++ categoria it
