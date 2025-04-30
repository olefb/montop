{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProcFS where

import Types hiding (ProcessInfo(..))
import qualified Types as Ty

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative ((<|>))
import Control.Monad (void)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import Control.Exception (try, IOException, SomeException(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT, catchE)
import Data.Either (partitionEithers)
import qualified System.Posix.User as Unix
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Set as Set
import Data.Word (Word64)
import Data.Char (isDigit)
import Types (ProcessInfo (..))
import qualified Data.Attoparsec.Combinator as A

data StatInfo = StatInfo
  { sPid :: Int
  , sComm :: T.Text
  , sState :: Char
  , sUtime :: Word64
  , sStime :: Word64
  , sCutime :: Word64
  , sCstime :: Word64
  } deriving (Show, Eq)

-- StatusInfo holds data parsed from /proc/[pid]/status
data StatusInfo = StatusInfo
  { stUid :: Maybe Int -- Use Maybe in case parsing fails
  , stVmRSS :: Maybe Int -- Memory in KiB
  } deriving (Show, Eq)

-- | Define the set of valid state characters
-- Source: https://man7.org/linux/man-pages/man5/proc_pid_stat.5.html
validStateChars :: Set.Set Char
validStateChars = Set.fromList "RSDZTtXxKWPI"

-- | Parser for /proc/[pid]/stat (fields 1-17)
-- Source: https://man7.org/linux/man-pages/man5/proc_pid_stat.5.html
parseStat :: A.Parser StatInfo
parseStat = do
    sPid' <- A.decimal <* A.space -- Field 1: pid<space>
    _ <- A.char '('               -- Consumes (

    -- Read command bytes until we see ") <StateChar>" via lookAhead
    commBytesList <- A.manyTill A.anyChar $ A.lookAhead $ do
        _ <- A.char ')'
        _ <- A.space
        _ <- A.satisfy (`Set.member` validStateChars) -- Look for a valid state char
        pure ()

    _ <- A.char ')' <* A.space
    sState' <- A.satisfy (`Set.member` validStateChars) <* A.space -- Consume State<space>

    -- Parse fields 4-13, consuming space after each
    (_ :: Int) <- A.decimal <* A.space -- Field 4: ppid
    (_ :: Int) <- A.decimal <* A.space -- Field 5: pgrp
    (_ :: Int) <- A.decimal <* A.space -- Field 6: session
    (_ :: Int) <- A.decimal <* A.space -- Field 7: tty_nr
    (_ :: Int) <- A.signed A.decimal <* A.space -- Field 8: tpgid
    (_ :: Word) <- A.decimal <* A.space -- Field 9: flags
    (_ :: Word64) <- A.decimal <* A.space -- Field 10: minflt
    (_ :: Word64) <- A.decimal <* A.space -- Field 11: cminflt
    (_ :: Word64) <- A.decimal <* A.space -- Field 12: majflt
    (_ :: Word64) <- A.decimal <* A.space -- Field 13: cmajflt

    -- Parse fields 14-17
    sUtime' <- A.decimal <* A.space  -- Field 14: utime
    sStime' <- A.decimal <* A.space  -- Field 15: stime
    sCutime' <- A.decimal <* A.space -- Field 16: cutime
    sCstime' <- A.decimal            -- Field 17: cstime

    let sComm' = decodeUtf8Lenient (B8.pack commBytesList)
    pure $ StatInfo sPid' sComm' sState' sUtime' sStime' sCutime' sCstime'

-- | Parser for /proc/[pid]/status (Key-Value pairs)
-- Looks for specific lines like "Uid:\t1000\t..." or "VmRSS:\t 4567 kB"
parseStatus :: A.Parser StatusInfo
parseStatus = do
    lines' <- B8.lines <$> A.takeByteString -- Read all content as lines
    let uidLine = findLine "Uid:" lines'
        rssLine = findLine "VmRSS:" lines'
    let uid = parseUid =<< uidLine
        maybeRss = parseRss =<< rssLine
    pure $ StatusInfo uid maybeRss
  where
    findLine :: BS.ByteString -> [BS.ByteString] -> Maybe BS.ByteString
    findLine prefix ls = case filter (prefix `BS.isPrefixOf`) ls of
                           (l:_) -> Just l -- Take first match
                           []    -> Nothing

    parseUid :: BS.ByteString -> Maybe Int
    parseUid line = case B8.words line of -- Split by whitespace
                      (_:uidStr:_) -> readMaybe (B8.unpack uidStr) -- Uid: Real Effective Saved Set
                      _            -> Nothing

    parseRss :: BS.ByteString -> Maybe Int
    parseRss line = case B8.words line of -- Split by whitespace
                      (_:rssStr:_) -> readMaybe (B8.unpack rssStr) -- VmRSS: Value kB
                      _            -> Nothing

parseProcStat :: A.Parser Word64
parseProcStat = do
    _ <- A.string "cpu" -- Match "cpu" prefix
    A.skipSpace
    -- Sum all numeric fields on the first line
    jiffies <- map (\s -> read (B8.unpack s) :: Word64) <$> A.sepBy1 (A.takeWhile1 isDigit) A.skipSpace
    A.skipWhile (/= '\n') -- Skip rest of the line
    void (A.char '\n') <|> A.endOfInput -- Consume newline or EOF
    pure $ sum jiffies

getSystemJiffies :: ExceptT AppError IO Word64
getSystemJiffies = do
    let statPath = "/proc/stat"
    content <- readFileExcept statPath `catchE` (\_ -> throwE $ ProcFSError (T.pack $ "Cannot read " ++ statPath))
    case A.parseOnly parseProcStat content of
        Left err -> throwE $ ProcFSError (T.pack $ "Error parsing " ++ statPath ++ ": " ++ err)
        Right jf -> pure jf


-- | Helper function to read a file, handling exceptions within ExceptT
readFileExcept :: FilePath -> ExceptT AppError IO BS.ByteString
readFileExcept path = do
    eContent <- liftIO $ try (BS.readFile path)
    case eContent of
        Left (e :: IOException) -> throwE $ ProcFSError (T.pack $ "Error reading " ++ path ++ ": " ++ show e)
        Right bs                -> pure bs

-- | Read and parse /proc/[pid]/stat
getStatInfo :: Int -> ExceptT AppError IO StatInfo
getStatInfo pid' = do
    let statPath = "/proc" </> show pid' </> "stat"
    content <- readFileExcept statPath `catchE` (\_ -> throwE $ ProcFSError (T.pack statPath))
    case A.parseOnly parseStat content of
        Left err     -> throwE $ ProcFSError (T.pack $ "Error parsing " ++ statPath ++ ": " ++ err)
        Right sInfo -> pure sInfo

-- | Read and parse /proc/[pid]/status
getStatusInfo :: Int -> ExceptT AppError IO StatusInfo
getStatusInfo pid' = do
    let statusPath = "/proc" </> show pid' </> "status"
    content <- readFileExcept statusPath
    case A.parseOnly parseStatus content of
        Left err      -> throwE $ ProcFSError (T.pack $ "Error parsing " ++ statusPath ++ ": " ++ err)
        Right stInfo -> pure stInfo

-- | Read /proc/[pid]/cmdline (arguments separated by null bytes)
readCmdline :: Int -> ExceptT AppError IO T.Text
readCmdline pid' = do
    let cmdlinePath = "/proc" </> show pid' </> "cmdline"
    content <- readFileExcept cmdlinePath
    -- Replace null bytes with spaces, decode as UTF-8 leniently
    pure $ T.replace "\NUL" " " $ TEnc.decodeUtf8Lenient content

-- | Map state character to a descriptive string
mapStateChar :: Char -> T.Text
mapStateChar s = case s of
    'R' -> "Running"
    'S' -> "Sleeping"
    'D' -> "Disk Sleep"
    'Z' -> "Zombie"
    'T' -> "Stopped"
    't' -> "Tracing stop"
    'X' -> "Dead"
    'x' -> "Dead"
    'K' -> "Wakekill"
    'W' -> "Waking"
    'P' -> "Parked"
    'I' -> "Idle"
    _   -> T.singleton s -- Unknown state

-- | Look up username from UID
lookupUsername :: Int -> ExceptT AppError IO T.Text
lookupUsername uid = do
    eUserEntry <- liftIO $ try (Unix.getUserEntryForID (fromIntegral uid))
    case eUserEntry of
        Left (_ :: SomeException) -> pure $ T.pack (show uid) -- Fallback to UID on error
        Right userEntry -> pure $ T.pack (Unix.userName userEntry)

-- | Get full ProcessInfo for a given PID
getProcessInfo :: Int -> ExceptT AppError IO (Ty.ProcessInfo, Word64)
getProcessInfo pid' = do
    statInfo <- getStatInfo pid'
    statusInfo <- getStatusInfo pid'
    cmd <- readCmdline pid' `catchE` (\_ -> pure $ "[" <> sComm statInfo <> "]")

    let stateText = mapStateChar (sState statInfo)
    let rssKiB = fromMaybe 0 (stVmRSS statusInfo)
    username <- case stUid statusInfo of
                  Just uid -> lookupUsername uid
                  Nothing  -> pure "unknown"

    -- Calculate total process jiffies
    let totalJiffies = sUtime statInfo + sStime statInfo + sCutime statInfo + sCstime statInfo

    -- Create the base ProcessInfo
    let pInfo = Ty.ProcessInfo
            { _pid = pid'
            , _comm = sComm statInfo
            , _state = stateText
            , _user = username
            , _rss = rssKiB
            , _cmdline = T.strip cmd
            , _cpuPercent = 0.0
            }
    pure (pInfo, totalJiffies)

-- | List all directories in /proc that look like PIDs
getPids :: IO (Either AppError [Int])
getPids = runExceptT $ do
    eEntries <- liftIO $ try (listDirectory "/proc")
    entries <- case eEntries of
      Left (e :: IOException) -> throwE $ ProcFSError (T.pack $ "Error listing /proc: " ++ show e)
      Right es -> pure es
    let pids = [ p | entry <- entries, Just p <- [readMaybe entry :: Maybe Int] ]
    if null pids
      then throwE $ ProcFSError "No processes found in /proc (permissions?)"
      else pure pids

-- | Fetch all processes and their jiffies
fetchAllProcesses :: IO (Either AppError [(Ty.ProcessInfo, Word64)])
fetchAllProcesses = runExceptT $ do
    pidsOrErr <- liftIO getPids
    pids <- case pidsOrErr of
        Left err -> throwE err
        Right ps -> pure ps

    results <- liftIO $ traverse (runExceptT . getProcessInfo) pids
    let (_, successes) = partitionEithers results
    pure successes