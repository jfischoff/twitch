![travis build status](https://travis-ci.org/jfischoff/twitch.svg?branch=master)

Twitch is monadic DSL and library for file watching.
It conveniently utilizes 'do' notation in the style of
[Shake](https://hackage.haskell.org/package/shake) and
[clay](https://hackage.haskell.org/package/clay) to expose the functionality of the
[fsnotify](http://hackage.haskell.org/package/fsnotify) cross-platform file system
watcher.

Here is an example that converts Markdown files to Html and reloads Safari
whenever the input files change.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Twitch
import Filesystem.Path.CurrentOS

main = defaultMain $ do
   "*.md"   |> \filePath -> system $ "pandoc -t html " ++ encodeString filePath
   "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"
```

Rules are specified in the `Dep` (for Dependency) monad. The library takes advantage
of the *OverloadedStrings* extension to create a Dep value from a glob pattern.

After creating a `Dep` value using a glob, event callbacks are added using prefix
or infix API.

There are three types of events, *add*, *modify* and *delete*. In many cases,
the *add* and *modify* responses are the same, so an 'add and modify' API
is provided

In the example above an 'add and modify' callback was added to both the "*.md"
and "*.html" globs using the `|>` operator.

All this is the common case, differing callbacks can be added with `|+` (or `add`)
and `|%` (or `modify`) functions. Finally, delete callbacks are added with
`|-` (of `delete`).

Here is a more complex usage example, handling all three events seperately.

```haskell
handleHaskellFiles :: Dep
handleHaskellFiles = "src/**/*.hs" |+ addToCabalFile |% reloadFile |- removeFromCabalFile
```

The glob above is also more complicated and incorporates a recursive wildcard. For
complete documentation on the glob syntax, consult the
[Glob](https://hackage.haskell.org/package/Glob-0.7.5/docs/System-FilePath-Glob.html#v:compile)
library's documentation.

Since a command pattern is calling system commands with a file path, a useful addition
to twitch is the [file-command-qq](https://hackage.haskell.org/package/file-command-qq-0.1.0.4) quasiquoter,
which is the package of the same name.

Here is a slightly more complicated version the example from earlier, using the
[file-command-qq](https://hackage.haskell.org/package/file-command-qq-0.1.0.4) quasiquoter.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Twitch
import FileCommand

main = defaultMain $ do
   "*.md"    |> [s|pandoc -t html -o$directory$basename-test.html $path|]
   "*.html"  |> [s|osascript refreshSafari.AppleScript|]
```
