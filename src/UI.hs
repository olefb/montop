{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI where

import Types
import Zipper (Zipper(..), zipperToList, focus)
import Brick
import Brick.Widgets.Border (borderWithLabel)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import qualified Data.Text as T
import Brick.Widgets.Center (vCenter)

-- | Define the columns to display and their order
-- (excluding CmdlineCol initially because it's variable width)
columnsToDisplay :: [SortColumn]
columnsToDisplay = [PidCol, UserCol, RssCol, CpuCol, StateCol, CommCol]

-- | Helper function to get the attribute for a column
columnToAttr :: SortColumn -> AttrName
columnToAttr PidCol     = pidAttr
columnToAttr UserCol    = userAttr
columnToAttr RssCol     = rssAttr
columnToAttr StateCol   = stateAttr
columnToAttr CommCol    = commAttr
columnToAttr CmdlineCol = cmdlineAttr
columnToAttr CpuCol     = cpuAttr

-- | Helper for padding text with spaces on the right
padAlignRight :: Int -> T.Text -> T.Text
padAlignRight maxWidth t = let len = T.length t in if len >= maxWidth then T.take maxWidth t else t <> T.replicate (maxWidth - len) " "

-- | Helper for padding text with spaces on the left
padAlignLeft :: Int -> T.Text -> T.Text
padAlignLeft maxWidth t = let len = T.length t in if len >= maxWidth then T.take maxWidth t else T.replicate (maxWidth - len) " " <> t

-- | Function to format the sort status
formatSortStatus :: SortColumn -> SortDirection -> T.Text
formatSortStatus col dir = T.pack $
    "Sort: " ++ show col ++ " (" ++ show dir ++ ") | Press 's' to cycle list, " ++
    "'d' to toggle asc/desc, 'f' to filter, 'x' to SIGTERM, 'q' to quit"

-- | Function to format the top border label based on mode
formatTopLabel :: AppState -> T.Text
formatTopLabel st =
    if st ^. filterMode
      -- In filter mode, show the filter text with a pseudo-cursor
      then "Filter: " <> (st ^. filterText) <> "_"
      else
        -- In normal mode, show sort status, and indicate if a filter is active
        let sortStatus = formatSortStatus (st ^. sortColumn) (st ^. sortDirection)
            filterIndicator = if T.null (st ^. filterText) then "" else " [FILTERED]"
        in sortStatus <> filterIndicator

drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    mZipper = st ^. processZipper
    errorPane = case st ^. errorMessage of
                  Nothing -> emptyWidget
                  Just msg -> vLimit 1 $ renderError msg

    -- Use the combined label function
    topLabel = formatTopLabel st

    -- Pass label text, viewport state to rendering function
    processWidget = case mZipper of
                      Nothing -> renderEmpty topLabel -- Pass label even when empty
                      Just z -> renderProcessZipper topLabel (st ^. processViewport) z

    ui = vBox [ processWidget , errorPane ]

-- | Accept Label Text, ViewportScroll state and use the viewport widget
renderProcessZipper :: T.Text -> ViewportScroll Name -> Zipper ProcessInfo -> Widget Name
renderProcessZipper topLabel _vpScroll z =
    -- Display dynamic label in the border
    borderWithLabel (txt topLabel) $
    viewport ProcessViewport Vertical $
    vBox renderedItems
   where
     items = zipperToList z
     focusedItem = focus z
     renderedItems = map (drawItem focusedItem) items
     drawItem currentFocus item = listDrawElement (item == currentFocus) item

-- | Render a message when the process list is empty, include dynamic label
renderEmpty :: T.Text -> Widget Name
renderEmpty topLabel = borderWithLabel (txt topLabel) $
              vCenter (txt "No matching processes found or error fetching.")

-- | Draw a single element in the process list
listDrawElement :: Bool -> ProcessInfo -> Widget Name
listDrawElement isSelected pInfo =
    let selHdr = if isSelected then withAttr selectedAttr . txt $ ">>" else txt "  "
        fixedColumnWidgets = map renderCol columnsToDisplay
          where
            renderCol col =
              let
                colData  = getColData col pInfo
                colWidth = getColWidth col
                colAttr  = columnToAttr col
                paddedData = if col == RssCol || col == CpuCol
                               then padAlignLeft colWidth colData
                               else padAlignRight colWidth colData
              in withAttr colAttr $ txt paddedData
        cmdlineWidget = withAttr cmdlineAttr $ txt (pInfo ^. cmdline)
    in hBox $ [selHdr, txt " "] ++ fixedColumnWidgets ++ [txt " ", cmdlineWidget]

renderError :: T.Text -> Widget Name
renderError msg = padLeftRight 1 $ withAttr errorAttr (txtWrap ("Error: " <> msg))

pidAttr, userAttr, rssAttr, stateAttr, commAttr, cmdlineAttr, cpuAttr, selectedAttr, errorAttr :: AttrName
pidAttr = attrName "pid"
userAttr = attrName "user"
rssAttr = attrName "rss"
stateAttr = attrName "state"
commAttr = attrName "comm"
cmdlineAttr = attrName "cmdline"
cpuAttr = attrName "cpu"
selectedAttr = attrName "selected"
errorAttr = attrName "error"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (selectedAttr,        V.black `on` V.yellow)
    , (pidAttr,             fg V.cyan)
    , (userAttr,            fg V.green)
    , (rssAttr,             fg V.magenta)
    , (stateAttr,           fg V.white)
    , (commAttr,            fg V.yellow)
    , (cmdlineAttr,         fg V.white)
    , (cpuAttr,             fg V.red)
    , (errorAttr,           fg V.red `V.withStyle` V.bold)
    ]