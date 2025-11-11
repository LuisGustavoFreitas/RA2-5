module Types where

import Data.Time.Clock (UTCTime)
import qualified Data.Map as M

data Item = Item
  { itemID    :: String
  , nome      :: String
  , quantidade:: Int
  , categoria :: String
  } deriving (Show, Read, Eq)

type Inventario = M.Map String Item

data AcaoLog
  = Add String
  | Remove String Int
  | Update String Int
  | Delete String 
  | List
  | Report
  | QueryFail String
  deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read, Eq)

type ResultadoOperacao = (Inventario, LogEntry)
