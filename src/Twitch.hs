-- | Twitch is a monadic DSL and library for file watching. 
--   It conveniently utilizes 'do' notation in the style of 
--   <https://hackage.haskell.org/package/shake Shake> and 
--   <https://hackage.haskell.org/package/clay clay> to expose the functionality of the
--   <http://hackage.haskell.org/package/fsnotify fsnotify> cross-platform file system 
--   watcher.
--   
--   Here is an example that converts Markdown files to HTML and reloads Safari
--   whenever the input files change.
--   
--   > {-# LANGUAGE OverloadedStrings #-}
--   > import Twitch 
--   > import Filesystem.Path.CurrentOS
--   > 
--   > main = defaultMain $ do
--   >   "*.md"   |> \filePath -> system $ "pandoc -t html " ++ encodeString filePath 
--   >   "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"
--   
--   Rules are specified in the 'Dep' (for Dependency) monad. The library takes advantage 
--   of the OverloadedStrings extension to create a 'Dep' value from a glob pattern.
--   
--   After creating a 'Dep' value using a glob, event callbacks are added using prefix
--   or infix API.
--   
--   There are three types of events: \'add\', \'modify\' and \'delete\'. In many cases, 
--   the \'add\' and \'modify\' responses are the same, so an \'add and modify\' API 
--   is provided
--   
--   In the example above, an \'add and modify\' callback was added to both the \"*.md\" 
--   and \"*.html\" globs using the '|>' operator. 
--   
--   Although this is the common case, differing callbacks can be added with '|+' (or 'add')
--   and '|%' (or 'modify') functions. Finally, delete callbacks are added with 
--   '|-' (of 'delete').
--   
--   Here is a more complex usage example, handling all three events separately.
--   
--   > handleHaskellFiles :: Dep 
--   > handleHaskellFiles = "src/**/*.hs" |+ addToCabalFile |% reloadFile |- removeFromCabalFile
--   
--   The glob above is also more complicated and incorporates a recursive wildcard. For
--   complete documentation on the glob syntax, consult the 
--   <https://hackage.haskell.org/package/Glob-0.7.5/docs/System-FilePath-Glob.html#v:compile Glob>
--   library's documentation.
--   
--   Since a command pattern is calling system commands with a file path, a useful addition
--   to twitch is the <https://hackage.haskell.org/package/file-command-qq-0.1.0.4 file-command-qq> quasiquoter. 
--
--   Here is a slightly more complicated version the example from earlier, using the 
--   FileCommand quasiquoter.
--   
--   > {-# LANGUAGE OverloadedStrings #-}
--   > {-# LANGUAGE QuasiQuotes #-}
--   > import Twitch 
--   > import FileCommand
--   >
--   > main = defaultMain $ do
--   >   "*.md"    |> [s|pandoc -t html -o$directory$basename-test.html $path|]
--   >   "*.html"  |> [s|osascript refreshSafari.AppleScript|]
--   
module Twitch 
  ( Dep
  , defaultMain
  -- * Infix API
  , (|+)
  , (|%)
  , (|-)
  , (|>)
  , (|#) 
  -- * Prefix API
  , add
  , modify
  , delete
  , addModify
  , name
  -- * Advanced Main
  , LoggerType (..)
  , Options (..)
  , defaultMainWithOptions
  -- * Running as a library
  , Issue (..)
  , Config (..)
  , run
  , runWithConfig
  -- * Extra
  , DepM
  ) where
import Twitch.Internal 
import Twitch.InternalRule (Config (..), Issue (..))
import Twitch.Main
import Twitch.Run
import Twitch.Rule (Rule, RuleAction, Name, PatternText)
