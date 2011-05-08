-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Maude
-- Copyright   :  (c) David Lazar, 2011
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This package provides a simple interface (via the 'rewrite' function) for
-- doing Maude rewrites from within Haskell.  The following steps are
-- performed every time the 'rewrite' function is executed: 
--
-- 1. A temporary file is created that contains the necessary commands.
--
-- 2. The temporary file (with any other input files) is executed by Maude.
--
-- 3. The temporary file is removed.
--
-- 4. The output from step 2 is parsed and returned.
--
-- This is a simple way to perform a single rewrite command, but it is
-- inefficient for performing many rewrite commands.  See /Future Work/
-- below.
-----------------------------------------------------------------------------

module Foreign.Maude
    (
    -- * Invoking Maude
      MaudeResult(..)
    , rewrite

    -- * Examples
    -- $examples

    -- * Future Work
    -- $future
    ) where

import Data.Char (isSpace)
import Data.List (break, stripPrefix)
import System.IO (hPutStrLn, hClose, openTempFile)
import System.Directory (getCurrentDirectory, removeFile)
import System.Process (readProcess)


-- | The result of a Maude rewrite.
data MaudeResult = MaudeResult
    { resultSort :: String  -- ^ The sort of the rewritten term.
    , resultTerm :: String  -- ^ The rewritten term.
    , statistics :: String  -- ^ Statistics printed by Maude.
    } deriving (Show)

-- | The name of the Maude executable.  It should be in $PATH.
maudeCmd :: FilePath
maudeCmd = "maude"

-- | Maude option flags which force Maude's output to be as relevant as
-- possible.
maudeArgs :: [String]
maudeArgs =
    [ "-no-banner"
    , "-no-advise"
    , "-no-wrap"
    , "-no-ansi-color"
    ]

-- | The 'rewrite' function takes a list of Maude file names and a term, and
-- attempts to rewrite the term in the context of those files.  It assumes
-- the command to run Maude is \"maude\" and that it is located somewhere in
-- @$PATH@.
rewrite :: [FilePath] -> String -> IO (Maybe MaudeResult)
rewrite files term = do
    runner <- newRunnerFile term
    let args = maudeArgs ++ files ++ [runner]
    out <- readProcess maudeCmd args []
    removeFile runner
    return $ parseMaudeResult out

-- | Parse Maude's output into a MaudeResult.  The current implementation
-- does very little sanity checking and can not parse Maude failures.
parseMaudeResult :: String -> Maybe MaudeResult
parseMaudeResult str = do
    let (stats, rest) = break (== '\n') str
    r <- stripPrefix "\nresult " rest
    let (sort, rest') = break (== ':') r
    let term = parseTerm rest'
    return $ MaudeResult
        { resultSort = sort
        , resultTerm = term
        , statistics = stats
        }
    where parseTerm = trim
                    . concat
                    . filter (/= "Bye.")
                    . lines
                    . drop 1    -- Remove the ':'
    
-- | Create a temporary file which contains the commands Maude should run at
-- startup, namely some formatting commands, the rewrite command, and quit.
newRunnerFile :: String -> IO FilePath
newRunnerFile term = do
    currDir <- getCurrentDirectory
    (tmpf, tmph) <- openTempFile currDir "runner.maude"
    hPutStrLn tmph "set print with parentheses on ."
    hPutStrLn tmph "set show command off ."
    hPutStrLn tmph $ "rewrite " ++ term ++ " ."
    hPutStrLn tmph "quit"
    hClose tmph
    return tmpf

-- | Remove leading and trailing whitespace from a string.
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

{- $examples

Maude's standard prelude is loaded by default, even when no input files are
specified:

>>> rewrite [] "not (A:Bool or B:Bool) implies (not A:Bool) and (not B:Bool)"
Just (MaudeResult
    { resultSort = "Bool"
    , resultTerm = "true"
    , statistics = "rewrites: 13 in 0ms cpu (0ms real) (~ rewrites/second)"
    })

The name of the module in which to reduce a term can be given explicitly:

>>> rewrite [] "in NAT-LIST : reverse(1 2 3 4)"
Just (MaudeResult
    { resultSort = "NeNatList"
    , resultTerm = "(4 (3 (2 1)))"
    , statistics = "rewrites: 6 in 0ms cpu (0ms real) (~ rewrites/second)"
    })

Notice that the resulting list is given in parenthesized form.  This is
because

> set print with parentheses on .

is executed before every rewrite.

Using a naive implementation of primes in Maude:

>>> rewrite ["primes.maude"] "2 .. 20"
Just (MaudeResult
    { resultSort = "PrimeSet"
    , resultTerm = "(2 (3 (5 (7 (11 (13 (17 19)))))))"
    , statistics = "rewrites: 185 in 0ms cpu (0ms real) (~ rewrites/second)"
    })

If we are only interested in the statistics:

>>> liftM statistics <$> rewrite ["primes.maude"] "2 .. 1000"
Just "rewrites: 409905 in 839ms cpu (856ms real) (488014 rewrites/second)"
-}

{- $future

This Maude interface is very minimal first step.  It could be extended in the
following ways:

* Better handling of Maude failures.  Failure messages should be parsed and
  returned to the user.

* The rewriting environment should be configurable.  This would allow
  parenthesized mode to be turned off and for the location of the Maude
  binary to be customized.

* Support for other Maude commands besides @rewrite@. 

* A Maude monad that handles failure and multiple Maude commands efficiently
  is a long-term goal for this package.
-}
