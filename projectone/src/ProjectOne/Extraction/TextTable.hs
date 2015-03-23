-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.Extraction.TextTable
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- This module outputs the DFA transition table in a human-readable format.
----------------------------------------------------------------------------
module ProjectOne.Extraction.TextTable (
  output
) where

import qualified Data.Set as S
import Data.Time
import ProjectOne.DFA
import ProjectOne.NFA

output :: DFA Int -> String
output dfa = "Accepting states: " ++ show a ++ "\n" ++
             "From\t\tVia\t\tWe can get to\n" ++ outputTable
  where
    NFA _ e _ a = getNFA dfa
    outputTable =
      unlines . S.toList $ S.map f e
      where
        f (Edge a' ch b') =
          show a' ++ "\t\t" ++ show ch ++ "\t\t" ++ show b'
        f (Epsilon a' b') = show a' ++ "\t\t\t\t" ++ show b'
