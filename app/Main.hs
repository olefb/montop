{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import UI
import Events
import ProcFS
import Zipper

import Brick
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Lens.Micro ((^?), _Just, (^.))
import qualified Data.Text as T
import qualified Brick.BChan as BChan
import Data.Maybe (listToMaybe)
import Data.Word (Word64)
import qualified Data.Map as Map
import Control.Monad.Trans.Except (runExceptT)

-- Choose cursor visibility based on filter mode
chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor st locs =
    if st ^. filterMode
       then listToMaybe locs -- Show cursor at the first available location
       else Nothing -- No cursor when not in filter mode

app :: App AppState CustomEvent Name
app = App
    { appDraw = drawUI
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = const theMap
    }

runDataFetcher :: BChan.BChan CustomEvent -> AppConfig -> IO ()
runDataFetcher chan config = forever $ do
    eProcsAndJiffies <- fetchAllProcesses -- Fetch the list of tuples
    case eProcsAndJiffies of
        Left err    -> BChan.writeBChan chan (ErrorFetching (renderAppError err))
        Right procs -> BChan.writeBChan chan (RefreshData procs) -- Pass the list of tuples directly
    threadDelay (refreshRate config)

initialState :: SortColumn -> SortDirection -> [(ProcessInfo, Word64)] -> AppState
initialState initialCol initialDir procsWithJiffies =
    let procs = map fst procsWithJiffies
        initialProcTimeMap = Map.fromList $ map (\(p, j) -> (p ^. pid, j)) procsWithJiffies
        sortedProcs = sortByCriteria initialCol initialDir procs
    in AppState
        { _processZipper = mkZipper sortedProcs
        , _errorMessage = Nothing
        , _processViewport = viewportScroll ProcessViewport
        , _sortColumn = initialCol
        , _sortDirection = initialDir
        , _filterText = T.empty
        , _filterMode = False
        , _prevProcTimes = initialProcTimeMap
        , _prevTotalSystemJiffies = 0
        }

main :: IO ()
main = do
    let config = AppConfig { refreshRate = 2000000 }
    let defaultSortColumn = PidCol
    let defaultSortDirection = Asc

    chan <- BChan.newBChan 10

    initialDataResult <- fetchAllProcesses
    let (initialProcsAndJiffies, initialError) = case initialDataResult of
          Left err -> ([], Just (renderAppError err))
          Right procs -> (procs, Nothing)

    -- Fetch initial system jiffies
    eInitialJiffies <- runExceptT getSystemJiffies
    let initialSystemJiffies = case eInitialJiffies of Right j -> j; Left _ -> 0

    -- Create initial state using the fetched data
    let st = initialState defaultSortColumn defaultSortDirection initialProcsAndJiffies
    let initialStateWithExtra = st { _errorMessage = initialError
                                   , _prevTotalSystemJiffies = initialSystemJiffies
                                   }

    _ <- forkIO $ runDataFetcher chan config
    initialVty <- mkVty V.defaultConfig
    let builder = mkVty V.defaultConfig
    finalState <- customMain initialVty builder (Just chan) app initialStateWithExtra

    putStrLn "Application exited. Final state focus:"
    print $ focus <$> (finalState ^? processZipper . _Just)