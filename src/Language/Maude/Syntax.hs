{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Maude.Syntax
-- Copyright   :  (c) David Lazar, 2012
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- Types representing Maude syntax and terms
-----------------------------------------------------------------------------
module Language.Maude.Syntax
    ( Term(..)
    ) where

import Data.Data

-- TODO: handle kinds

data Term
    = Term
        { termSort     :: String
        , termOp       :: String
        , termChildren :: [Term]
        }
    -- ^ Generic representation of terms in Maude
    | IterTerm
        { termSort     :: String
        , termOp       :: String
        , termChildren :: [Term]
        , iterations   :: Integer
        }
    -- ^ Term constructed from an iterated (@iter@) operator,
    -- for example, the @s_@ constructor for Nats
    deriving (Eq, Ord, Show, Data, Typeable)
