{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Twitch.Main where
import Prelude hiding (log, FilePath)
import Data.Monoid ( (<>) )
import Options.Applicative
    ( Applicative((<*>))
    , (<$>)
    , Parser
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
    ( IOMode(AppendMode),
      Handle,
      hPrint,
      openFile,
      hClose )
import Data.Foldable ( for_ )
import Twitch.Run ( runWithConfig )
import Twitch.Internal ( Dep )
import System.Directory ( getCurrentDirectory )
import Data.Maybe ( fromMaybe )
import Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import Control.Monad ( liftM )
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
    handle <- openFile (F.encodeString filePath) AppendMode
    return (hPrint handle, Just handle)
  NoLogger -> return (const $ return (), Nothing)

data Options = Options
  { log                       :: LoggerType
  -- ^ The logger type.
  --   This cooresponds to the --log or -l argument. The valid options
  --   are "LogToStdout", "LogToFile", and "NoLogger"
  --   If "LogToFile" a file can provide with the 'logFile' field.
  , logFile                   :: Maybe FilePath
  -- ^ The file to log to
  --   This is only used if the 'log' field is set to "LogToFile".
  --   This cooresponds to the --log-file or -f argument
  , root                      :: Maybe FilePath
  -- ^ The root directory to watch.
  --   This cooresponds to the --root and -r argument.
  --   By default this is empty and the current directory is used
  , recurseThroughDirectories :: Bool
  -- ^ If true, main will recurse throug all subdirectories of the 'dirsToWatch'
  --   field. Otherwise the 'dirsToWatch' will be used literally.
  --   By default this is true, and disabled with the --no-recurse-flag
  , debounce                  :: DebounceType
  -- ^ This corresponds to the debounce type used in the fsnotify library
  --   The argument for default main is --debounce or -b .
  --   Valid options are "DebounceDefault", "Debounce", "NoDebounce"
  --   If "Debounce" is used then a debounce amount must be specified with the
  --   'debounceAmount'
  , debounceAmount            :: Double
  -- ^ The amount to debounce. This is only meaningful when 'debounce' is set
  --   to 'Debounce'.
  --   It cooresponds to the --debounce-amount or -a argument
  , pollInterval              :: Int
  -- ^ poll interval if polling is used.
  --   This cooresponds to the --poll-interval or -i argument
  , usePolling                :: Bool
  -- ^ Sets polling to true if used
  --   This cooresponds to the --should-poll or -p flag
  }

data DebounceType
  = DebounceDefault
  | Debounce
  | NoDebounce
  deriving (Eq, Show, Read, Ord)

-- use the new vinyl for the options
-- the you get the monoid instance for free

instance Default Options where
  def = Options
    { log                       = NoLogger
    , logFile                   = Nothing
    , root                      = Nothing
    , recurseThroughDirectories = True
    , debounce                  = DebounceDefault
    , debounceAmount            = 0
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
  let filePath = F.decodeString $ stripDoubleQuotes xs
  in if F.valid filePath then
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
        ( long "debounce"
       <> short 'b'
       <> metavar "DEBOUNCE"
       <> help "Type of debouncing. Valid choices are DebounceDefault | Debounce | NoDebounce"
       <> value (debounce def)
        )
  <*> option auto
        ( long "debounce-amount"
       <> short 'a'
       <> metavar "DEBOUNCE_AMOUNT"
       <> help "Target for the greeting"
       <> value (debounceAmount def)
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

toDB :: Real a => a -> DebounceType -> FS.Debounce
toDB amount dbtype = case dbtype of
  DebounceDefault -> FS.DebounceDefault
  Debounce        -> FS.Debounce $ fromRational $ toRational amount
  NoDebounce      -> FS.NoDebounce

makeAbsolute :: F.FilePath -> F.FilePath -> F.FilePath
makeAbsolute currentDir path = 
  if F.relative path then
      currentDir F.</> path
  else 
      path

optionsToConfig :: Options -> IO (FilePath, IR.Config, Maybe Handle)
optionsToConfig Options {..} = do
  currentDir <- F.decodeString <$> getCurrentDirectory
  let root' = makeAbsolute currentDir $ fromMaybe currentDir root

  (logger, mhandle) <- toLogger (fromMaybe "log.txt" logFile) log
  dirsToWatch <- if recurseThroughDirectories then
                   (root' :) <$> findAllDirs root'
                 else
                   return [root']

  let watchConfig = FS.WatchConfig
        { FS.confDebounce     = toDB debounceAmount debounce
        , FS.confPollInterval = pollInterval
        , FS.confUsePolling   = usePolling
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
  putStrLn "Type anything to quit"
  _ <- getLine
  for_ mhandle hClose
  FS.stopManager manager



