{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Twitch.Main where
import Control.Applicative -- satisfy GHC < 7.10
import Control.Concurrent ( threadDelay )
import Control.Exception ( finally )
import Data.Monoid
import Options.Applicative
  ( Parser
  , helper
  , execParser
  , value
  , short
  , progDesc
  , option
  , metavar
  , long
  , info
  , help
  , header
  , fullDesc
  , auto
  , eitherReader
  , switch
  , flag
  , ParserInfo
  )
import Data.Default ( Default(..) )
import qualified System.FSNotify as FS
import Twitch.Path ( findAllDirs )
import qualified Twitch.InternalRule as IR
import System.IO
  ( IOMode(AppendMode)
  , Handle
  , hPrint
  , openFile
  , hClose
  )
import Data.Foldable ( for_ )
import Twitch.Run ( runWithConfig )
import Twitch.Internal ( Dep )
import System.Directory ( getCurrentDirectory )
import Data.Maybe ( fromMaybe )
import System.FilePath
  ( FilePath
  , (</>)
  , isRelative
  , isValid
  )
import Control.Monad ( forever, liftM )
-- Moved here to suppress redundant import warnings for GHC > 7.10
import Prelude hiding (log, FilePath)
-- parse the command line
--

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b] 
concatMapM f = liftM concat . mapM f

data LoggerType
  = LogToStdout
  | LogToFile
  | NoLogger
  deriving (Eq, Show, Read, Ord)

toLogger :: FilePath
         -> LoggerType
         -> IO (IR.Issue -> IO (), Maybe Handle)
toLogger filePath lt = case lt of
  LogToStdout -> return (print, Nothing)
  LogToFile -> do
    handle <- openFile filePath AppendMode
    return (hPrint handle, Just handle)
  NoLogger -> return (const $ return (), Nothing)

data Options = Options
  { log                       :: LoggerType
  -- ^ The logger type.
  --   This corresponds to the --log or -l argument. The valid options
  --   are "LogToStdout", "LogToFile", and "NoLogger"
  --   If "LogToFile" a file can provide with the 'logFile' field.
  , logFile                   :: Maybe FilePath
  -- ^ The file to log to.
  --   This is only used if the 'log' field is set to "LogToFile".
  --   This corresponds to the --log-file or -f argument.
  , root                      :: Maybe FilePath
  -- ^ The root directory to watch.
  --   This corresponds to the --root and -r argument.
  --   By default this is empty and the current directory is used.
  , recurseThroughDirectories :: Bool
  -- ^ If true, main will recurse through all subdirectories of the 'dirsToWatch'
  --   field. Otherwise the 'dirsToWatch' will be used literally.
  --   By default this is true, and disabled with the --no-recurse-flag .
  , pollInterval              :: Int
  -- ^ poll interval if polling is used.
  --   This corresponds to the --poll-interval or -i argument.
  , usePolling                :: Bool
  -- ^ Sets polling to true if used.
  --   This corresponds to the --should-poll or -p flag.
  }

-- use the new vinyl for the options
-- the you get the monoid instance for free

instance Default Options where
  def = Options
    { log                       = NoLogger
    , logFile                   = Nothing
    , root                      = Nothing
    , recurseThroughDirectories = True
    , pollInterval              = 10^(6 :: Int) -- 1 second
    , usePolling                = False
    }

dropDoubleQuotes :: String -> String
dropDoubleQuotes [] = []
dropDoubleQuotes (x : xs) 
  | x == '\"' = xs
  | otherwise = x : xs

stripDoubleQuotes :: String -> String
stripDoubleQuotes = dropDoubleQuotes . reverse . dropDoubleQuotes . reverse
    
-- strip quotes if they are there
readFilePath :: String -> Either String FilePath
readFilePath xs = 
  let filePath = stripDoubleQuotes xs
  in if isValid filePath then
        Right filePath
     else
        Left $ "invalid filePath " ++ xs 

pOptions :: Parser Options
pOptions
   =  Options
  <$> option auto
        ( long "log"
       <> short 'l'
       <> metavar "LOG_TYPE"
       <> help "Type of logger. Valid options are LogToStdout | LogToFile | NoLogger"
       <> value (log def)
        )
  <*> option (Just <$> eitherReader readFilePath)
        ( long "log-file"
       <> short 'f'
       <> metavar "LOG_FILE"
       <> help "Log file"
       <> value (logFile def)
        )
  <*> option (Just <$> eitherReader readFilePath)
        ( long "root"
       <> short 'r'
       <> metavar "ROOT"
       <> help "Root directory to watch"
       <> value (root def)
        )
  <*> flag True False
        ( long "no-recurse"
       <> short 'n'
       <> help "flag to turn off recursing"
        )
  <*> option auto
        ( long "poll-interval"
       <> short 'i'
       <> metavar "POLL_INTERVAL"
       <> help "Poll interval if polling is used"
       <> value (pollInterval def)
        )
  <*> switch
        ( long "should-poll"
       <> short 'p'
       <> help "Whether to use polling or not. Off by default"
        )

-- This is like run, but the config params can be over written from the defaults

makeAbsolute :: FilePath -> FilePath -> FilePath
makeAbsolute currentDir path = 
  if isRelative path then
      currentDir </> path
  else 
      path

optionsToConfig :: Options -> IO (FilePath, IR.Config, Maybe Handle)
optionsToConfig Options {..} = do
  currentDir <- getCurrentDirectory
  let root' = makeAbsolute currentDir $ fromMaybe currentDir root

  (logger, mhandle) <- toLogger (fromMaybe "log.txt" logFile) log
  dirsToWatch <- if recurseThroughDirectories then
                   (root' :) <$> findAllDirs root'
                 else
                   return [root']

  let watchConfig = FS.defaultConfig
        { FS.confThreadingMode      = FS.SingleThread
        , FS.confWatchMode          = if usePolling 
            then FS.WatchModePoll { FS.watchModePollInterval = pollInterval } 
            else FS.WatchModeOS
        , FS.confOnHandlerException = const $ pure ()
        }

  let config = IR.Config
        { logger      = logger
        , dirs        = dirsToWatch
        , watchConfig = watchConfig
        }
  return (root', config, mhandle)

opts :: ParserInfo Options
opts = info (helper <*> pOptions)
      ( fullDesc
     <> progDesc "twitch"
     <> header "a file watcher"
      )

-- | Simplest way to create a file watcher app. Set your main equal to defaultMain
--   and you are good to go. See the module documentation for examples.
--
--   The command line is parsed to make 'Options' value. For more information on
--   the arguments that can be passed see the doc for 'Options' and the run the
--   executable made with defaultMain with the --help argument.
defaultMain :: Dep -> IO ()
defaultMain dep = do
  options <- execParser opts
  defaultMainWithOptions options dep

-- | A main file that uses manually supplied options instead of parsing the passed in arguments.
defaultMainWithOptions :: Options -> Dep -> IO ()
defaultMainWithOptions options dep = do
  (root, config, mhandle) <- optionsToConfig options
  manager <- runWithConfig root config dep
  putStrLn "Type Ctrl+C to quit"
  forever (threadDelay maxBound)
    `finally` do
      for_ mhandle hClose
      FS.stopManager manager

