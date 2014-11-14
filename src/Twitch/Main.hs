{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Twitch.Main where
import Data.Monoid
import Options.Applicative
import Data.Default
import qualified System.FSNotify as FS
import Twitch.Path
import qualified Twitch.InternalRule as IR
import System.IO
import Data.Foldable (for_)
import Twitch.Run
import Twitch.Internal
import System.Directory
import Data.Maybe
import Prelude hiding (log)
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F
import Data.Time.Clock
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent
-- parse the command line
-- 


concatMapM f = fmap concat . mapM f

data LoggerType 
  = LogToStdout
  | LogToFile
  | NoLogger
  deriving (Eq, Show, Read, Ord)

toLogger :: FilePath 
         -> LoggerType
         -> IO (IR.Issue -> IO (), Maybe Handle)
toLogger filePath = \case
  LogToStdout -> return (print, Nothing)
  LogToFile -> do 
    handle <- openFile filePath AppendMode
    return (hPutStrLn handle . show, Just handle)
  NoLogger -> return (const $ return (), Nothing)

data Options = Options 
  { log          :: LoggerType
  , logFile      :: Maybe FilePath
  -- ^ A logger for the issues 
  , dirsToWatch  :: [FilePath]
  -- ^ The directories to watch
  , recurseThroughDirectories :: Bool
  , debounce     :: DebounceType
  , debounceAmount :: Double
  -- ^ Debounce configuration
  , pollInterval :: Int
  -- ^ poll interval
  , usePolling   :: Bool
  -- ^ config for the file watch
  , currentDir   :: Maybe FilePath
  }

data DebounceType 
  = DebounceDefault
  | Debounce
  | NoDebounce
  deriving (Eq, Show, Read, Ord)

instance Default Options where
  def = Options
    { log                       = NoLogger
    , logFile                   = Nothing
    , dirsToWatch               = []
    , recurseThroughDirectories = True
    , debounce                  = DebounceDefault
    , debounceAmount            = 0
    , pollInterval              = 10^(6 :: Int) -- 1 second
    , usePolling                = False
    , currentDir                = Nothing
    }

pOptions :: Parser Options
pOptions 
   =  Options
  <$> option
        ( long "log"
       <> short 'l'
       <> metavar "LOG_TYPE"
       <> help "Type of logger. Valid options are LogToStdout | LogToFile | NoLogger" 
       <> value (log def)
        )
  <*> option
        ( long "log-file"
       <> short 'f'
       <> metavar "LOG_FILE"
       <> help "Log file" 
       <> value (logFile def)
        )
  <*> option
        ( long "directories"
       <> short 'd'
       <> metavar "DIRECTORIES"
       <> help "Directories to watch"
       <> value (dirsToWatch def)
        )
  <*> option
        ( long "recurse"
       <> short 'r'
       <> metavar "RECURSE"
       <> help "Boolean to recurse or directories or not" 
       <> value (recurseThroughDirectories def)
        )
  <*> option
        ( long "debounce"
       <> short 'b'
       <> metavar "DEBOUNCE"
       <> help "Target for the greeting" 
       <> value (debounce def)
        )
  <*> option
        ( long "debounce-amount"
       <> short 'a'
       <> metavar "DEBOUNCE_AMOUNT"
       <> help "Target for the greeting" 
       <> value (debounceAmount def)
        )
  <*> option
        ( long "poll-interval"
       <> short 'i'
       <> metavar "POLL_INTERVAL"
       <> help "Poll interval if polling is used"
       <> value (pollInterval def)
        )
  <*> option
        ( long "poll"
       <> short 'p'
       <> metavar "POLL"
       <> help "Whether to use polling or not" 
       <> value (usePolling def)
        )
  <*> nullOption 
        ( long "current-dir"
       <> short 'c'
       <> metavar "CURRENT_DIR"
       <> help "Director to append to the glob patterns" 
       <> value (currentDir def)
       <> eitherReader (return . Just)
        )

-- This is like run, but the config params can be over written from the defaults

toDB amount = \case
  DebounceDefault -> FS.DebounceDefault
  Debounce        -> FS.Debounce $ fromRational $ toRational amount
  NoDebounce      -> FS.NoDebounce

optionsToConfig :: Options -> IO (FilePath, IR.Config, Maybe Handle)
optionsToConfig Options {..} = do 
  actualCurrentDir <- getCurrentDirectory
  let currentDir' = fromMaybe actualCurrentDir currentDir
      dirsToWatch' = if null dirsToWatch then
                       [currentDir']
                     else
                       dirsToWatch                 
  print dirsToWatch'
                       
  (logger, mhandle) <- toLogger (fromMaybe "log.txt" logFile) log 
  let encodedDirs = map F.decodeString dirsToWatch'
  dirsToWatch'' <- if recurseThroughDirectories then 
                   (encodedDirs ++) <$> concatMapM findAllDirs encodedDirs
                 else 
                   return encodedDirs
  
  let watchConfig = FS.WatchConfig
        { FS.confDebounce     = toDB debounceAmount debounce
        , FS.confPollInterval = pollInterval
        , FS.confUsePolling   = usePolling
        }
  
  let config = IR.Config
        { log         = logger
        , dirsToWatch = dirsToWatch''
        , watchConfig = watchConfig
        }
  return (currentDir', config, mhandle)

defaultMain :: Dep -> IO ()
defaultMain dep = do
  let opts = info (helper <*> pOptions)
        ( fullDesc
       <> progDesc "Print a greeting for TARGET"
       <> header "hello - a test for optparse-applicative" 
        )
  (currentDir, config, mhandle) <- optionsToConfig =<< execParser opts
  let currentDir' = F.decodeString currentDir
  manager <- runWithConfig currentDir' config dep 
  putStrLn "Type anything to quit"
  _ <- getLine
  for_ mhandle hClose
  FS.stopManager manager
  