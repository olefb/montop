{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Events where

import Types
import Zipper
import Brick
import qualified Graphics.Vty as V
import Lens.Micro
import Data.Maybe (isJust, fromMaybe)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Ord (comparing)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import ProcFS (getSystemJiffies)
import qualified Data.Map as Map
import Data.Word (Word64)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Posix.Types (ProcessID)
import Control.Exception (try, SomeException)
import Data.Map (Map)
import Control.Monad.Trans.Except (runExceptT)

compareProcessInfo :: SortColumn -> ProcessInfo -> ProcessInfo -> Ordering
compareProcessInfo col = case col of
    PidCol     -> comparing _pid
    UserCol    -> comparing _user
    RssCol     -> comparing _rss
    StateCol   -> comparing _state
    CommCol    -> comparing _comm
    CmdlineCol -> comparing _cmdline
    CpuCol     -> comparing _cpuPercent

-- | Invert ordering for Descending sort
invertOrdering :: Ordering -> Ordering
invertOrdering GT = LT
invertOrdering LT = GT
invertOrdering EQ = EQ

-- | Apply direction to a comparison result
applySortDirection :: SortDirection -> Ordering -> Ordering
applySortDirection Asc  ord = ord
applySortDirection Desc ord = invertOrdering ord

-- | Sort a list of processes based on current criteria
sortByCriteria :: SortColumn -> SortDirection -> [ProcessInfo] -> [ProcessInfo]
sortByCriteria col dir = L.sortBy comparer
  where
    comparer p1 p2 = applySortDirection dir (compareProcessInfo col p1 p2)

-- Filter predicate
matchesFilter :: T.Text -> ProcessInfo -> Bool
matchesFilter filt p =
    let lowFilt = T.toLower filt
    in T.isInfixOf lowFilt (T.toLower $ p ^. comm) ||
       T.isInfixOf lowFilt (T.toLower $ p ^. cmdline)

-- | Takes raw process data + jiffies, current system jiffies, and calculates CPU%
updateZipperAndCalculateCPU :: [(ProcessInfo, Word64)] -> Word64 -> EventM Name AppState ()
updateZipperAndCalculateCPU procsWithJiffies currentTotalSystemJiffies = do
    -- Get previous state
    prevProcTimesMap <- gets (^. prevProcTimes)
    prevSystemJiffies <- gets (^. prevTotalSystemJiffies)

    -- Calculate system delta, prevent division by zero
    let systemDelta = if currentTotalSystemJiffies > prevSystemJiffies
                      then currentTotalSystemJiffies - prevSystemJiffies
                      else 0

    -- Calculate CPU % for each process
    let (updatedProcs, nextProcTimesMap) = calculateCpuPercentages systemDelta prevProcTimesMap procsWithJiffies

    -- Get sort/filter criteria
    col <- gets (^. sortColumn)
    dir <- gets (^. sortDirection)
    filt <- gets (^. filterText)

    -- Sort and filter the list after CPU% has been calculated
    let sortedProcs = sortByCriteria col dir updatedProcs
    let filteredProcs = if T.null filt
                          then sortedProcs
                          else filter (matchesFilter filt) sortedProcs

    -- Get old focus PID
    mOldZipper <- gets (^. processZipper)
    let mOldPid = mOldZipper <&> (\z -> focus z ^. pid)

    let mNewZipper = mkZipper filteredProcs

    -- Restore focus
    let mRestoredZipper = case (mOldPid, mNewZipper) of
          (Just oldPid, Just newZ) -> Just $ findAndFocus (\p -> p ^. pid == oldPid) newZ
          (_, newZ) -> newZ

    -- Update the state (Zipper, Error, and previous times)
    modify $ (processZipper .~ mRestoredZipper)
           . (errorMessage .~ Nothing)
           . (prevProcTimes .~ nextProcTimesMap) -- Store the new map for next time
           . (prevTotalSystemJiffies .~ currentTotalSystemJiffies) -- Store current system jiffies

    -- Ensure focus is visible
    ensureFocusVisible

calculateCpuPercentages :: Word64             -- Total system jiffies delta
                        -> Map Int Word64     -- Previous process jiffies map
                        -> [(ProcessInfo, Word64)] -- Current process info & jiffies
                        -> ([ProcessInfo], Map Int Word64) -- Updated ProcessInfos, Next process jiffies map
calculateCpuPercentages systemDelta prevMap = foldr processOne ([], Map.empty)
  where
    processOne :: (ProcessInfo, Word64)       -- Current process data
               -> ([ProcessInfo], Map Int Word64) -- Accumulator
               -> ([ProcessInfo], Map Int Word64)
    processOne (pInfo, currentProcJiffies) (accProcs, accMap) =
      let currentPid = pInfo ^. pid
          mPrevProcJiffies = Map.lookup currentPid prevMap
          -- Use '_' to ignore the first element of the tuple (the calculated percentage itself)
          -- We only need the ProcessInfo with the updated lens value.
          (_, pInfoWithCpu) = case mPrevProcJiffies of
            Just prevProcJiffies ->
              let procDelta = if currentProcJiffies > prevProcJiffies
                              then currentProcJiffies - prevProcJiffies
                              else 0
                  calculatedPerc = if systemDelta > 0
                                      then 100.0 * fromIntegral procDelta / fromIntegral systemDelta
                                      else 0.0
              -- Return tuple
              in (calculatedPerc, pInfo & cpuPercent .~ calculatedPerc)
            Nothing ->
              -- Return tuple
              (0.0, pInfo & cpuPercent .~ 0.0)

      -- Use the updated ProcessInfo and insert current times into the next map
      in (pInfoWithCpu : accProcs, Map.insert currentPid currentProcJiffies accMap)

-- | Resort the current display list
resortAndUpdateZipper :: EventM Name AppState ()
resortAndUpdateZipper = do
    mZipper <- gets (^. processZipper)
    when (isJust mZipper) $ do
        let currentZipper = fromMaybe (error "Zipper disappeared unexpectedly") mZipper
        let procs = zipperToList currentZipper -- These procs have potentially stale CPU%
        let mOldFocusedPid = Just (focus currentZipper ^. pid)

        newCol <- gets (^. sortColumn)
        newDir <- gets (^. sortDirection)
        filt <- gets (^. filterText)

        -- Re-sort and re-filter the existing list
        let sortedProcs = sortByCriteria newCol newDir procs
        let filteredProcs = if T.null filt then sortedProcs else filter (matchesFilter filt) sortedProcs

        let mNewZipper = mkZipper filteredProcs
        let mRestoredZipper = case (mOldFocusedPid, mNewZipper) of
              (Just oldPid, Just newZ) -> Just $ findAndFocus (\p -> p ^. pid == oldPid) newZ
              (_, newZ) -> newZ

        modify (processZipper .~ mRestoredZipper)
        ensureFocusVisible

-- | Scrolls the viewport so the currently focused item in the zipper
-- is at the top of the viewport. Does nothing if the zipper is empty.
ensureFocusVisible :: EventM Name AppState ()
ensureFocusVisible = do
    mZipper <- gets (^. processZipper)
    case mZipper of
      Nothing -> pure () -- No zipper, nothing to scroll to
      Just z -> do
          let focusIdx = zipperFocusIndex z -- Get the 0-based index of the focused item
          let vp = viewportScroll ProcessViewport
          vScrollToBeginning vp  -- First, scroll to the absolute top
          vScrollBy vp focusIdx  -- Then, scroll down by the focus index

-- | Helper IO Action to send SIGTERM
sendSignalToPid :: Int -> IO (Either String ()) -- Return Either for error message or success
sendSignalToPid p = do
    let pId = fromIntegral p :: ProcessID
    putStrLn $ "[IO] Sending SIGTERM to PID " ++ show pId -- Debug output to terminal
    eresult <- try (signalProcess sigTERM pId)
    case eresult of
        Left (e :: SomeException) -> do
            let errMsg = "[IO] Error sending SIGTERM to PID " ++ show pId ++ ": " ++ show e
            putStrLn errMsg -- Also print error to terminal
            pure $ Left errMsg
        Right () -> do
            putStrLn $ "[IO] SIGTERM sent successfully to PID " ++ show pId
            pure $ Right ()

-- Main event handler
handleEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
    inFilterMode <- gets (^. filterMode)
    if inFilterMode
      then do -- Esc in filter mode: cancel filtering
          modify $ (filterMode .~ False) . (filterText .~ T.empty)
          resortAndUpdateZipper -- Re-apply (now empty) filter
      else -- Esc not in filter mode: quit
          halt

handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
    inFilterMode <- gets (^. filterMode)
    unless inFilterMode halt -- Quit only if not filtering

-- Handle VTY events based on mode
handleEvent (VtyEvent ev) = do
    inFilterMode <- gets (^. filterMode)
    if inFilterMode
      then handleFilterEvent ev
      else handleNormalEvent ev

-- Handle AppEvents (Data Refresh / Error)
handleEvent (AppEvent (RefreshData procsWithJiffies)) = do -- This pattern expects the list of tuples
    ejiffies <- liftIO $ runExceptT getSystemJiffies
    case ejiffies of
      Left appErr -> modify (errorMessage ?~ renderAppError appErr)
      Right currentSystemJiffies ->
        -- This function call must receive the list of tuples
        updateZipperAndCalculateCPU procsWithJiffies currentSystemJiffies

handleEvent (AppEvent (ErrorFetching msg)) = do
    modify (errorMessage ?~ msg)

-- Catch-all (e.g., Mouse events)
handleEvent _ = pure ()

-- | Handler for when filterMode is True
handleFilterEvent :: V.Event -> EventM Name AppState ()
handleFilterEvent ev =
    case ev of
      -- Finish filtering
      V.EvKey V.KEnter [] -> do
          modify (filterMode .~ False)

      -- Delete character
      V.EvKey V.KBS [] -> do
          modify $ filterText %~ (\t -> if T.null t then T.empty else T.init t)
          -- Re-apply filter immediately as text changes
          resortAndUpdateZipper

      -- Add character
      V.EvKey (V.KChar c) [] -> do
          modify $ filterText %~ (`T.snoc` c)
          -- Re-apply filter immediately as text changes
          resortAndUpdateZipper

      -- Ignore other keys in filter mode
      _ -> pure ()


-- Handler for when filterMode is False
handleNormalEvent :: V.Event -> EventM Name AppState ()
handleNormalEvent ev = do
    let vp = viewportScroll ProcessViewport
    let pageSize = 10

    case ev of
      V.EvKey (V.KChar 'x') [] -> do
          mZipper <- gets (^. processZipper)
          case mZipper of
              Nothing -> pure () -- No process selected
              Just z -> do
                  let targetPid = focus z ^. pid
                  -- Perform the IO action using liftIO
                  resultOrError <- liftIO $ sendSignalToPid targetPid
                  -- Update state based on the result within EventM
                  case resultOrError of
                       Left ioErrMsg -> modify (errorMessage ?~ T.pack ioErrMsg)
                       Right ()      -> do
                        modify (errorMessage ?~ T.pack ("Sent SIGTERM to PID " ++ show targetPid))
                        modify (filterText .~ T.empty)
                        resortAndUpdateZipper

      -- Enter filter mode
      V.EvKey (V.KChar 'f') [] -> modify (filterMode .~ True)

      -- Sorting keys
      V.EvKey (V.KChar 's') [] -> do
          modify $ sortColumn %~ (\c -> if c == maxBound then minBound else succ c)
          resortAndUpdateZipper

      V.EvKey (V.KChar 'd') [] -> do
          modify $ sortDirection %~ toggleDirection
          resortAndUpdateZipper

      -- Navigation keys
      V.EvKey V.KUp [] -> do
          modify $ processZipper %~ fmap zipperUp
          vScrollBy vp (-1)
      V.EvKey V.KDown [] -> do
          modify $ processZipper %~ fmap zipperDown
          vScrollBy vp 1
      V.EvKey V.KPageUp []   -> do
           modify $ processZipper %~ fmap (zipperPageUp pageSize)
           vScrollBy vp (-pageSize)
      V.EvKey V.KPageDown [] -> do
           modify $ processZipper %~ fmap (zipperPageDown pageSize)
           vScrollBy vp pageSize
      V.EvKey V.KHome [] -> do
           vScrollToBeginning vp
      V.EvKey V.KEnd [] -> do
           vScrollToEnd vp

      -- Ignore other keys in normal mode
      _ -> pure ()
