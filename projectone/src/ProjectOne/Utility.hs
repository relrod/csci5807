-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.Utility
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Functions which tie together the other bits of logic.
----------------------------------------------------------------------------
module ProjectOne.Utility (
) where

import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import ProjectOne.DFA
import ProjectOne.NFA
import ProjectOne.RegexRule hiding (Class)
import ProjectOne.Input.RegexParser
import ProjectOne.Input.SpecLineParser

classNameToCharset :: String -> [SpecLine] -> Maybe String
classNameToCharset s xs = listToMaybe [ set | Class name set <- xs, name == s ]
