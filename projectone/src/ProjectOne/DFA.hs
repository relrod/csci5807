-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.DFA
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Functions for converting from an 'NFA' to a 'DFA'.
----------------------------------------------------------------------------
module ProjectOne.DFA (
  DFA (..)
) where

import qualified Data.Set as S
import ProjectOne.NFA

newtype DFA a = DFA (NFA a)
