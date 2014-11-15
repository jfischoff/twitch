-- | Twitch is monadic DSL and library for file watching. 
--   It conveniently utilizes 'do' notation in the style of 
--   <https://hackage.haskell.org/package/shake Shake> and 
--   <https://hackage.haskell.org/package/clay clay> to expose the functionality of
--   <http://hackage.haskell.org/package/fsnotify fsnotify> cross-platform file system 
--   watcher.
--   
--   Here is an example that converts Markdown files to Html and reloads Safari
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
--   of the OverloadedStrings extension to create a Dep value from a glob pattern.
--   
--   After creating a 'Dep' value using a glob, event callbacks are added using prefix
--   or infix API.
--   
--   There are three types of events, \'add\', \'modify\' and \'delete\'. In many cases, 
--   the add and modify responses are the same, so a \'add and modify\' portion of the API 
--   is provided
--   
--   In the example above an \'add and modify\' callback was added to both the \"*.md\" 
--   and \"*.html\" globs using the '|>' operator. 
--   
--   All this is the common case, differing callbacks can be added with '|+' (or 'add')
--   and '|%' (or 'modify') functions. Finally, delete callbacks are added with 
--   '|-' (of 'delete').
--   
--   Here is a more complex usage example, handling all three events seperately.
--   
--   > handleHaskellFiles :: Dep 
--   > handleHaskellFiles = "src/**/*.hs" |+ addToCabalFile |% reloadFile |- removeFromCabalFile
--   
--   The glob above is also more complicated and incorporates a recursive wildcard. For
--   complete documentation on the glob syntax, consult the 
--   <https://hackage.haskell.org/package/Glob-0.7.5/docs/System-FilePath-Glob.html#v:compile Glob>
--   library's documentation.
--   
module Twitch 
  ( Dep
  , DepM
  , run
  , defaultMain
  , (|+)
  , (|%)
  , (|-)
  , (|>)
  , (|#) 
  , add
  , modify
  , delete
  , addModify
  ) where
import Twitch.Internal 
import Twitch.Main
import Twitch.Run
import Twitch.Rule (Rule, RuleAction, Name, PatternText)