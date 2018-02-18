module Language.Maude.Exec.XML
    ( Parser(..)
    , parseRewriteResult
    , parseSearchResults
    ) where

import Control.Monad
import Text.XML.Light

import Language.Maude.Syntax
import Language.Maude.Exec.Types

data Parser a
    = ParseError Element String
    | Ok a
    deriving (Show)

instance Applicative Parser where
    pure = Ok
    (Ok f) <*> (Ok a) = Ok $ f a
    (ParseError e s) <*> _ = ParseError e s
    _ <*> (ParseError e s) = (ParseError e s)    
             
instance Functor Parser where
    fmap _ (ParseError e s) = ParseError e s
    fmap f (Ok r) = Ok (f r)

instance Monad Parser where
    return = Ok
    ParseError e s >>= _ = ParseError e s
    Ok r >>= k = k r

-- <maudeml> (root)
parseRewriteResult :: Element -> Parser RewriteResult
parseRewriteResult e = parseRewriteResult' =<< child "result" e

-- <maudeml> (root)
parseSearchResults :: Element -> Parser SearchResults
parseSearchResults e = case results of
    [] -> ParseError e "no search results found"
    _  -> mapM parseSearchResult =<< actualResults
  where
    results = findChildren (unqual "search-result") e
    actualResults = filterM notNone results
    notNone e' = attr "solution-number" e' >>= return . (/= "NONE")

-- <result>
parseRewriteResult' :: Element -> Parser RewriteResult
parseRewriteResult' e = do
    stats <- parseStatistics e
    term <- case elChildren e of
        [e'] -> parseTerm e'
        _ -> ParseError e "expecting exactly one result child"
    return $ RewriteResult
        { resultTerm = term
        , rewriteStatistics = stats
        }

-- <result>
parseStatistics :: Element -> Parser MaudeStatistics
parseStatistics e = do
    rews <- readattr "total-rewrites" e
    real <- readattr "real-time-ms" e
    cpu  <- readattr "cpu-time-ms" e
    return $ MaudeStatistics rews real cpu

-- <search-result>
parseSearchResult :: Element -> Parser SearchResult
parseSearchResult e = do
    solution <- readattr "solution-number" e
    stats <- parseStatistics e
    subst <- parseSubstitution =<< child "substitution" e
    return $ SearchResult
        { searchSolutionNumber = solution
        , searchStatistics = stats
        , searchResult = subst
        }

-- <substitution>
parseSubstitution :: Element -> Parser Substitution
parseSubstitution e = do
    assignment <- child "assignment" e
    -- TODO: better error handling
    [s, t] <- mapM parseTerm (elChildren assignment)
    return $ Substitution s t

-- <term>
parseTerm :: Element -> Parser Term
parseTerm e = do
    sort <- parseSort e
    op <- attr "op" e
    children <- mapM parseTerm (elChildren e)
    case findAttr (unqual "number") e of
        Nothing -> return $ Term sort op children
        Just i -> return $ IterTerm sort op children (read i)

-- <term>
parseSort :: Element -> Parser String
parseSort e = fromMaybeM err $ sort `mplus` synt
  where
    sort = findAttr (unqual "sort") e
    synt = findAttr (unqual "syntactic-sort") e
    err = ParseError e "key 'sort' or 'syntactic-sort' not found"

{- utility functions -}

child :: String -> Element -> Parser Element
child tag e = fromMaybeM err $ findChild (unqual tag) e
  where
    err = ParseError e $ "child not found: <" ++ tag ++ ">"

attr :: String -> Element -> Parser String
attr key e = fromMaybeM err $ findAttr (unqual key) e
  where
    err = ParseError e $ "key not found: " ++ key

readattr :: Read a => String -> Element -> Parser a
readattr key e = do
    s <- attr key e
    case reads s of
        [(a, "")] -> return a
        _ -> ParseError e $ "failed to read value of key: " ++ key

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM err = maybe err return
