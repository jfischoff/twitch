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
--   Here is another complex example, using the named `addModify` and `delete` callbacks
--   to the same function, which build a pdf and a Word document using pandoc, and
--   refreshes a mupdf window.
--
--   ```haskell
--   buildPDFandWordandRefreshWindow _ = do
--     pdfLatexCode <- system "pdflatex --interaction errorstopmode -file-line-error -halt-on-error document.tex"
--     (pandocCode,pandocOut,pandocErr) <- readProcessWithExitCode "pandoc" [ "--from=latex" , "--to=docx" , "document.tex" , "-o" , "document.docx" ] ""
--     (xwininfoCode,xwininfoOut,xwininfoErr) <- readProcessWithExitCode "xwininfo" ["-root", "-int", "-all"] ""
--     let windowId = head . words . head . filter (isInfixOf "document") $ lines xwininfoOut
--     (xDoToolCode,xDoToolOut,xDoToolErr) <- readProcessWithExitCode "xdotool" ["key", "--window", windowId, "r"] ""
--     return ()
--
--   main :: IO ()
--   main = defaultMain $ do
--     addModify buildPDFandWordandRefreshWindow "src/**/*.tex"
--     delete    buildPDFandWordandRefreshWindow "src/**/*.tex"
--   ```
--
--   The globs in the above two examples are also more complicated and incorporate recursive wildcards. For
--   complete documentation on the glob syntax, consult the
--   [Glob](https://hackage.haskell.org/package/Glob-0.7.5/docs/System-FilePath-Glob.html#v:compile)
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
  , DebounceType (..)
  , LoggerType (..)
  , Options (..)
  , defaultMainWithOptions
  -- * Running as a library
  , Issue (..)
  , InternalRule
  , Rule
  , RuleIssue
  , Config (..)
  , run
  , runWithConfig
  -- * Extra
  , DepM
  ) where
import Twitch.Internal
    ( Dep,
      DepM,
      (|+),
      (|%),
      (|-),
      (|>),
      (|#),
      add,
      modify,
      delete,
      addModify,
      name )
import Twitch.InternalRule ( Config(..), Issue(..), InternalRule )
import Twitch.Main
    ( DebounceType(..),
      Options(Options, debounce, debounceAmount, root, log,
              logFile, pollInterval, recurseThroughDirectories, usePolling),
      LoggerType(..),
      defaultMain,
      defaultMainWithOptions )
import Twitch.Run ( run, runWithConfig )
import Twitch.Rule ( Rule, RuleIssue )
