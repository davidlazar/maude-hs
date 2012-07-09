{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Maude.Exec
-- Copyright   :  (c) David Lazar, 2012
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- This package provides a simple interface to the Maude executable for
-- doing Maude rewrites from within Haskell.
--
-- Note: Maude is considered to have failed if it ever prints to stderr.
-----------------------------------------------------------------------------

module Language.Maude.Exec
    (
      module Language.Maude.Exec.Types

    -- * High-level interface
    , rewrite
    , search

    -- * Low-level interface
    , runMaude
    , defaultConf
    ) where

import Control.Exception
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hClose)
import System.IO.Temp
import System.Directory (getCurrentDirectory)
import System.Exit
import System.Process.Text (readProcessWithExitCode)
import Text.XML.Light (parseXMLDoc)

import Language.Maude.Exec.Types
import Language.Maude.Exec.XML

-- | @rewrite files term@ rewrites @term@ using Maude (with @files@ loaded).
--
-- This function may throw a 'MaudeException'.
rewrite :: [FilePath] -> Text -> IO RewriteResult
rewrite files term = do
    let cmd = Rewrite term
    maudeResult <- runMaude defaultConf{ loadFiles = files } cmd
    let maybeXml = parseXMLDoc $ maudeXmlLog maudeResult
    xml <- maybe (throwIO LogToXmlFailure) return maybeXml
    case parseRewriteResult xml of
        ParseError e s -> throwIO $ XmlToResultFailure s e
        Ok a -> return a

-- | @search files term pattern@ uses Maude (with @files@ loaded) to search
-- for all reachable states starting from @term@ and matching the given
-- @pattern@. Note that @pattern@ should also include the search type.
-- For example,
--
-- >>> search [] term "=>! N:Nat"
--
-- runs the Maude command @search term =>! N:Nat@.
--
-- This function may throw a 'MaudeException'.
search :: [FilePath] -> Text -> Text -> IO SearchResults
search files term pattern = do
    let cmd = Search term pattern
    maudeResult <- runMaude defaultConf{ loadFiles = files } cmd
    let maybeXml = parseXMLDoc $ maudeXmlLog maudeResult
    xml <- maybe (throwIO LogToXmlFailure) return maybeXml
    case parseSearchResults xml of
        ParseError e s -> throwIO $ XmlToResultFailure s e
        Ok a -> return a

-- | @runMaude conf cmd@ performs the Maude command @cmd@ using the
-- configuration @conf@.
--
-- This function may throw a 'MaudeException'.
runMaude :: MaudeConf -> MaudeCommand -> IO MaudeResult
runMaude conf cmd = do
    currDir <- getCurrentDirectory
    withTempFile currDir "maudelog.xml" $ \xmlFile xmlHandle -> do
        hClose xmlHandle -- we don't need it
        let exe = maudeCmd conf
        let args = maudeArgs xmlFile ++ (loadFiles conf)
        let input = mkMaudeInput cmd
        (exitCode, out, err) <- readProcessWithExitCode exe args input
        when (not (T.null err) || exitCode /= ExitSuccess) $
            throwIO $ MaudeFailure
                { maudeFailureExitCode = exitCode
                , maudeFailureStderr = err
                , maudeFailureStdout = out
                }
        xmlText <- T.readFile xmlFile
        return $ MaudeResult
            { maudeStdout = out
            , maudeXmlLog = xmlText
            }

-- | Default Maude configuration
defaultConf :: MaudeConf
defaultConf = MaudeConf
    { maudeCmd  = "maude"
    , loadFiles = []
    }

-- | Maude flags which force its output to be as relevant as possible
maudeArgs :: FilePath -> [String]
maudeArgs xmlFile =
    [ "-no-banner"
    , "-no-advise"
    , "-no-wrap"
    , "-no-ansi-color"
    , "-xml-log=" ++ xmlFile
    ]

-- | Constructs the text that is sent to Maude.
mkMaudeInput :: MaudeCommand -> Text
mkMaudeInput cmd = T.unlines $
    [ "set show command off ."
    , showCommand cmd
    , " ."
    , "quit"
    ]

showCommand :: MaudeCommand -> Text
showCommand (Rewrite term) = "rewrite " `T.append` term
showCommand (Erewrite term) = "erewrite " `T.append` term
showCommand (Search term pattern) = T.concat ["search ", term, " ", pattern]
