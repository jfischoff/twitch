#! /usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes      #-}
import Control.Applicative
import System.Process
import Control.Monad
import Filesystem.Path
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import Twitch
import FileCommand

main :: IO ()
main = defaultMain $ do
  "doodles.md" |> print
--       [s|pandoc -o$directory$basename.html -s -fmarkdown+hard_line_breaks+footnotes+raw_html --highlight-style kate -c $directory$basename.css --to=html --smart $path|]
--       [s|pandoc -o$directory$basename.html -s -fmarkdown+hard_line_breaks+footnotes+raw_html $path|]
--  "doodles.css"  |> [s| osascript refreshSafari.AppleScript |]
--  "doodles.html" |> [s| osascript refreshSafari.AppleScript |]
  
  
  
  