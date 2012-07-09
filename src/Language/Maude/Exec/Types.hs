{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Maude.Exec.Types
-- Copyright   :  (c) David Lazar, 2012
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- Types shared between modules in the Language.Maude.Exec namespace
-----------------------------------------------------------------------------
module Language.Maude.Exec.Types
    (
    -- * Error handling
      MaudeException(..)

    -- * Configuring Maude's execution
    , MaudeCommand(..)
    , MaudeConf(..)

    -- * Result types
    , MaudeResult(..)
    , RewriteResult(..)
    , SearchResult(..)
    , SearchResults
    , Substitution(..)
    , MaudeStatistics(..)
    ) where

import Control.Exception
import Data.Data
import Data.Text (Text)
import System.Exit (ExitCode)
import qualified Text.XML.Light as XML

import Language.Maude.Syntax

data MaudeException
    = MaudeFailure
        { maudeFailureExitCode :: ExitCode
        , maudeFailureStderr   :: Text
        , maudeFailureStdout   :: Text
        }
    -- ^ Thrown when the @maude@ executable fails
    | LogToXmlFailure
    -- ^ Thrown when the log produced by Maude is not parseable as XML
    | XmlToResultFailure String XML.Element
    -- ^ Thrown when the XML can't be parsed/translated to
    -- one of the result types below
    deriving (Typeable)

instance Exception MaudeException

instance Show MaudeException where
    showsPrec _ (MaudeFailure e err out)
        = showString "MaudeFailure (exitCode = " . showsPrec 0 e
        . showString ") (stderr = " . showsPrec 0 err
        . showString ") (stdout = " . showsPrec 0 out . showChar ')'
    showsPrec _ LogToXmlFailure = showString "LogToXmlFailure"
    showsPrec _ (XmlToResultFailure s e)
        = showString "XmlToResultFailure " . showsPrec 0 s
        . showString "\n" . showString (take n xml)
        . showString (if length xml > n then "..." else "")
      where
        n = 100
        xml = XML.showElement e

-- | Commands performed by Maude
data MaudeCommand
    = Rewrite Text
    | Erewrite Text
    | Search Text Text
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Configuration of Maude's execution
data MaudeConf = MaudeConf
    { maudeCmd    :: FilePath
    -- ^ Path to the Maude executable
    , loadFiles   :: [FilePath]
    -- ^ Files to load before running a command
    } deriving (Eq, Ord, Show, Data, Typeable)

-- | Low-level Maude result
data MaudeResult
    = MaudeResult
        { maudeStdout :: Text
        -- ^ Text printed to standard out during execution
        , maudeXmlLog :: Text
        -- ^ XML log obtained via Maude's @--xml-log@ option
        }
    deriving (Eq, Ord, Show, Data, Typeable)

-- | High-level (e)rewrite result
data RewriteResult = RewriteResult
    { resultTerm        :: Term
    -- ^ The rewritten term
    , rewriteStatistics :: MaudeStatistics
    -- ^ Statistics about the rewrite performed
    } deriving (Eq, Ord, Show, Data, Typeable)

-- | High-level search result
data SearchResult = SearchResult
    { searchSolutionNumber :: Integer
    , searchStatistics     :: MaudeStatistics
    , searchResult         :: Substitution
    } deriving (Eq, Ord, Show, Data, Typeable)

-- | Several search results
type SearchResults = [SearchResult]

-- | Search result substitution
data Substitution = Substitution Term Term
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Statistics returned by Maude after a successful command
data MaudeStatistics = MaudeStatistics
    { totalRewrites :: Integer
    -- ^ Total rewrites performed
    , realTime      :: Integer
    -- ^ Real time (milliseconds)
    , cpuTime       :: Integer
    -- ^ CPU time (milliseconds)
    } deriving (Eq, Ord, Show, Data, Typeable)
