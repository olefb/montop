{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-} -- To easily use record fields in generator
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SortTests (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
import qualified Data.Text as T

import Types (ProcessInfo(..), SortColumn(..), SortDirection(..))
import Events (sortByCriteria, compareProcessInfo, applySortDirection)

-- | Generator for plausible PIDs (positive Ints)
genPid :: Gen Int
genPid = getPositive <$> arbitrary

-- | Generator for plausible command/cmdline Text (non-empty)
genNonEmptyText :: Gen T.Text
genNonEmptyText = T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['0'..'9'] ++ " ()-_/")

-- | Generator for plausible RSS values (non-negative)
genRss :: Gen Int
genRss = getNonNegative <$> arbitrary

-- Arbitrary instance for ProcessInfo
instance Arbitrary ProcessInfo where
    arbitrary = do
        -- Generate components using specific generators or defaults
        _pid <- genPid
        _comm <- genNonEmptyText
        _state <- genNonEmptyText -- State text can be arbitrary for sorting tests
        _user <- genNonEmptyText
        _rss <- genRss
        _cmdline <- genNonEmptyText
        let _cpuPercent = 0.0
        -- Create the ProcessInfo record
        return ProcessInfo{..} -- Uses RecordWildCards

    -- shrink is useful for finding minimal failing cases (optional but good)
    shrink p@ProcessInfo{..} =
        -- Shrink components individually
        [ p { _pid = pid' } | pid' <- shrink _pid, pid' > 0 ] ++
        [ p { _comm = T.pack comm' } | comm' <- shrink (T.unpack _comm), not (null comm') ] ++
        [ p { _state = T.pack state' } | state' <- shrink (T.unpack _state), not (null state') ] ++
        [ p { _user = T.pack user' } | user' <- shrink (T.unpack _user), not (null user') ] ++
        [ p { _rss = rss' } | rss' <- shrink _rss, rss' >= 0 ] ++
        [ p { _cmdline = T.pack cmdline' } | cmdline' <- shrink (T.unpack _cmdline), not (null cmdline') ]

-- Arbitrary instance for SortColumn
instance Arbitrary SortColumn where
    arbitrary = arbitraryBoundedEnum -- Uses Enum/Bounded

-- Arbitrary instance for SortDirection
instance Arbitrary SortDirection where
    arbitrary = elements [Asc, Desc]

-- | Helper to check if a list is sorted according to a comparison function
isSortedBy :: (a -> a -> Ordering) -> [a] -> Bool
isSortedBy _ [] = True
isSortedBy _ [_] = True
isSortedBy cmp (x:y:xs) = cmp x y /= GT && isSortedBy cmp (y:xs)

spec :: Spec
spec = do
    describe "sortByCriteria" $ do
        it "preserves the length of the list" $
            property $ \(col :: SortColumn) (dir :: SortDirection) (procs :: [ProcessInfo]) ->
                length procs == length (sortByCriteria col dir procs)

        it "produces a list with the same elements (is a permutation)" $
            property $ \(col :: SortColumn) (dir :: SortDirection) (procs :: [ProcessInfo]) ->
                -- Sort both lists using the standard Ord instance for ProcessInfo, then check equality.
                L.sort procs == L.sort (sortByCriteria col dir procs)

        it "produces a list sorted according to the criteria" $
            property $ \(col :: SortColumn) (dir :: SortDirection) (procs :: [ProcessInfo]) ->
                let sortedProcs = sortByCriteria col dir procs
                    -- Define the specific comparer for this test case
                    comparer :: ProcessInfo -> ProcessInfo -> Ordering
                    comparer p1 p2 = applySortDirection dir (compareProcessInfo col p1 p2)
                in isSortedBy comparer sortedProcs -- Pass the fully defined comparer

        it "is idempotent (sorting a sorted list doesn't change it)" $
             property $ \(col :: SortColumn) (dir :: SortDirection) (procs :: [ProcessInfo]) ->
                let sortedOnce = sortByCriteria col dir procs
                    sortedTwice = sortByCriteria col dir sortedOnce
                in sortedOnce == sortedTwice