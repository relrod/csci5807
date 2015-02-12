-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.NFA
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- This module exports functions and types for working with 'NFA's.
----------------------------------------------------------------------------
module ProjectOne.NFA (
  NFA (..)
, Edge (..)
) where

import Data.Set

-- | Here we represent a nondeterministic finite automata (NFA).
data NFA a = NFA { states    :: Set a
                 , edges     :: Set (Edge a)
                 , initial   :: a
                 , accepting :: Set a
                 } deriving (Eq, Ord, Show)

-- | This type defines edges that appear in our graphs. An edge can either be a
-- character or ε (the empty string). Right now we are using the literal 'Char'
-- type that Haskell gives us. In reality, we should probably generalize this
-- more, but that isn't necessary to complete the assignment.
data Edge a = Edge a Char a
            | Epsilon a a
            deriving (Eq, Ord, Show)
