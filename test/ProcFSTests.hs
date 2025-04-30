{-# LANGUAGE OverloadedStrings #-}

module ProcFSTests (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Either (isRight)
import Data.List (isPrefixOf)
import Data.Word (Word64)

import ProcFS (parseStat, parseStatus, mapStateChar, StatInfo(..), StatusInfo(..))

-- | Generator for realistic command names
genRealisticComm :: Gen T.Text
genRealisticComm = T.pack <$> oneof
    [ listOf1 (elements alphaNum) -- Simple alphanumeric
    , arbitrary `suchThat` (not . null) -- Any non-empty string (might contain spaces/symbols)
    , addParens <$> arbitrary `suchThat` (not . null) -- Add parentheses inside
    ]
  where
    alphaNum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    addParens s = "(" ++ s ++ ")"

-- | Generator for valid process state characters
genStateChar :: Gen Char
genStateChar = elements "RSDZTWtXIKP"

-- | Generator for valid /proc/[pid]/stat input and the expected StatInfo result
genStatInput :: Gen (B8.ByteString, StatInfo)
genStatInput = do
    pid <- getPositive <$> arbitrary
    commT <- genRealisticComm
    stateC <- genStateChar
    utime <- getNonNegative <$> arbitrary :: Gen Word64
    stime <- getNonNegative <$> arbitrary :: Gen Word64
    cutime <- getNonNegative <$> arbitrary :: Gen Word64
    cstime <- getNonNegative <$> arbitrary :: Gen Word64

    let commEscaped = TEnc.encodeUtf8 commT

    let fields = B8.unwords [ B8.pack (show pid)
                            , "(" <> commEscaped <> ")"
                            , B8.singleton stateC
                            , "0 0 0 0 0 0 0 0 0 0"
                            , B8.pack (show utime)
                            , B8.pack (show stime)
                            , B8.pack (show cutime)
                            , B8.pack (show cstime)
                            , "0 0 0 0 0"
                            ]
    let input = fields <> "\n"

    let expected = StatInfo { sPid = pid
                            , sComm = commT
                            , sState = stateC
                            , sUtime = utime
                            , sStime = stime
                            , sCutime = cutime
                            , sCstime = cstime
                            }
    -- Return the result from the do block
    return (input, expected)

-- | Generator for /proc/[pid]/status input and expected StatusInfo
genStatusInput :: Gen (B8.ByteString, StatusInfo)
genStatusInput = do
    maybeUid <- arbitrary :: Gen (Maybe (Positive Int))
    maybeRss <- arbitrary :: Gen (Maybe (Positive Int))

    -- Generate some other plausible lines
    otherLines <- listOf $ B8.pack <$> suchThat arbitrary (\s -> not ("Uid:" `isPrefixOf` s || "VmRSS:" `isPrefixOf` s || null s))

    let uidLine = case maybeUid of
                    Nothing -> []
                    Just (Positive uid) -> [B8.pack $ "Uid:\t" ++ show uid ++ "\t" ++ show uid ++ "\t" ++ show uid ++ "\t" ++ show uid]
    let rssLine = case maybeRss of
                    Nothing -> []
                    Just (Positive rss) -> [B8.pack $ "VmRSS:\t" ++ show rss ++ " kB"]

    -- Shuffle all lines together
    allLines <- shuffle (otherLines ++ uidLine ++ rssLine)

    let input = B8.unlines allLines
    let expected = StatusInfo (getPositive <$> maybeUid) (getPositive <$> maybeRss)

    return (input, expected)

spec :: Spec
spec = do
    describe "parseStat" $ do
        it "successfully parses valid stat input" $
            property $ forAll genStatInput $ \(input, _) ->
                isRight (A.parseOnly parseStat input)

        it "extracts the correct data from valid stat input" $
            property $ forAll genStatInput $ \(input, expected) ->
                case A.parseOnly parseStat input of
                    Left err -> counterexample ("Parse failed: " ++ err) False
                    Right result -> result === expected -- Check if parsed result matches generated expectation

        it "parses command with internal parentheses correctly (using fixed parser)" $
            -- Provide dummy values for fields 4 through 17
            let input = "123 (a(b)c) S 1 2 3 4 5 6 7 8 9 10 11 12 13 14\n" -- pid (comm) state ppid ... cstime
                expectedComm = T.pack "a(b)c"
                expectedState = 'S'
                -- Expected jiffies based on input above (fields 14-17 are 11 12 13 14)
                expectedUtime = 11
                expectedStime = 12
                expectedCutime = 13
                expectedCstime = 14
            in case A.parseOnly parseStat input of
                 Left err -> counterexample ("Parse failed: " ++ err) False
                 Right result ->
                     (sComm result === expectedComm) .&&.
                     (sState result === expectedState) .&&.
                     (sUtime result === expectedUtime) .&&.
                     (sStime result === expectedStime) .&&.
                     (sCutime result === expectedCutime) .&&.
                     (sCstime result === expectedCstime)

    describe "parseStatus" $ do
        it "successfully parses valid status input (may be empty)" $
            property $ forAll genStatusInput $ \(input, _) ->
                -- Check if it's Right OR if the input was truly empty and resulted in Nothing fields
                -- Status parser should always succeed structurally, even if fields are missing.
                isRight (A.parseOnly parseStatus input)

        it "extracts Uid and VmRSS correctly when present" $
            property $ forAll genStatusInput $ \(input, expected) ->
                case A.parseOnly parseStatus input of
                     Left err -> counterexample ("Parse failed: " ++ err) False
                     Right result -> result === expected -- Check Maybe Ints

        it "handles missing Uid field" $
            let input = "Name: test\nState: R\nVmRSS: 123 kB\n"
                expected = StatusInfo Nothing (Just 123)
            in A.parseOnly parseStatus input === Right expected

        it "handles missing VmRSS field" $
            let input = "Name: test\nState: R\nUid: 1000 1000 1000 1000\n"
                expected = StatusInfo (Just 1000) Nothing
            in A.parseOnly parseStatus input === Right expected

    -- Test mapStateChar
    describe "mapStateChar" $ do
        it "maps known state characters" $ do
            mapStateChar 'R' `shouldBe` "Running"
            mapStateChar 'S' `shouldBe` "Sleeping"
        it "returns unknown characters as singleton Text" $ do
            mapStateChar '?' `shouldBe` "?"
            mapStateChar '1' `shouldBe` "1"