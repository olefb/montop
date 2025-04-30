{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified ProcFSTests
import qualified ZipperTests
import qualified SortTests

main :: IO ()
main = hspec $ do
    describe "ProcFS Tests" ProcFSTests.spec
    describe "Zipper Tests" ZipperTests.spec
    describe "Sort Tests" SortTests.spec