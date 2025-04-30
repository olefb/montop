{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Brick ( ViewportScroll )
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import GHC.Generics (Generic)
import Zipper ( Zipper )
import Numeric (showFFloat)
import Control.Lens (view)
import Data.Map (Map)
import Data.Word (Word64)
import Text.Printf (printf)
import Data.Default
import Data.Text (Text, pack)

data ProcessInfo = ProcessInfo
  { _pid :: Int
  , _comm :: Text
  , _state :: Text
  , _user :: Text
  , _rss :: Int
  , _cmdline :: Text
  , _cpuPercent :: Double
  } deriving (Show, Eq, Ord, Generic)

-- Define a default value for initialization or when CPU% can't be calculated
instance Default ProcessInfo where
    def = ProcessInfo 0 "" "" "" 0 "" 0.0

makeLenses ''ProcessInfo

data Name = ProcessViewport
          deriving (Eq, Ord, Show)

-- Define Sortable Columns
data SortColumn = PidCol | UserCol | RssCol | StateCol | CommCol | CmdlineCol | CpuCol
                deriving (Eq, Show, Enum, Bounded)

-- Define Sort Direction
data SortDirection = Asc | Desc
                   deriving (Eq, Show)

toggleDirection :: SortDirection -> SortDirection
toggleDirection Asc = Desc
toggleDirection Desc = Asc

-- The application state using Zipper
data AppState = AppState
  { _processZipper          :: Maybe (Zipper ProcessInfo)
  , _errorMessage           :: Maybe Text
  , _processViewport        :: ViewportScroll Name
  , _sortColumn             :: SortColumn
  , _sortDirection          :: SortDirection
  , _filterText             :: Text
  , _filterMode             :: Bool
  , _prevProcTimes          :: Map Int Word64
  , _prevTotalSystemJiffies :: Word64
  }

makeLenses ''AppState

data CustomEvent = RefreshData [(ProcessInfo, Word64)]
                 | ErrorFetching Text
newtype AppConfig = AppConfig { refreshRate :: Int }
data AppError = ProcFSError Text | ConfigError Text | LookupError Text deriving (Show)

renderAppError :: AppError -> Text
renderAppError (ProcFSError t) = "ProcFS Error: " <> t
renderAppError (ConfigError t) = "Config Error: " <> t
renderAppError (LookupError t) = "Lookup Error: " <> t

-- Type class for abstracting column rendering details
class RenderableColumn col where
    getColData :: col -> ProcessInfo -> Text
    getColWidth :: col -> Int
    getColTitle :: col -> Text

-- Instance for the SortColumn type
instance RenderableColumn SortColumn where
    getColData PidCol     = pack . show . view pid
    getColData UserCol    = view user
    getColData RssCol     = \pInfo ->
                                let rssKiB = pInfo ^. rss
                                    rssMiB :: Double = fromIntegral rssKiB / 1024.0
                                in pack (showFFloat (Just 1) rssMiB "") <> "M"
    getColData StateCol   = view state
    getColData CommCol    = view comm
    getColData CmdlineCol = view cmdline
    getColData CpuCol     = \pInfo ->
                                pack $ printf "%5.1f" (pInfo ^. cpuPercent)

    getColWidth PidCol     = 7
    getColWidth UserCol    = 9
    getColWidth RssCol     = 9
    getColWidth StateCol   = 12
    getColWidth CommCol    = 25
    getColWidth CmdlineCol = 0
    getColWidth CpuCol     = 6

    getColTitle PidCol     = pack "PID"
    getColTitle UserCol    = pack "USER"
    getColTitle RssCol     = pack "RSS"
    getColTitle StateCol   = pack "STATE"
    getColTitle CommCol    = pack "COMM"
    getColTitle CmdlineCol = pack "COMMAND"
    getColTitle CpuCol     = pack "%CPU"