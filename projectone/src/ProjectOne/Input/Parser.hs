-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.Input.Parser
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Here is where we parse the input specification into forms that we can
-- manipulate internally.
----------------------------------------------------------------------------
module ProjectOne.Input.Parser (
  parseSpecLine
) where

import Text.Parser.Char

-- | We use the @parsers@ abstraction library atop the @trifecta@ parser
-- combinator library but __only__ for parsing input lines. Notably,
-- page 3 of the project specification says to not touch the regex with any
-- any library, so we make a point here of only doing sanity checks on it and
-- not actually treating it as a regex in any manner. The sanity checks we do
-- are as follows:
--
--    * (None yet)
parseSpecLine :: (Monad m, CharParsing m) => m ()
parseSpecLine = error ""
