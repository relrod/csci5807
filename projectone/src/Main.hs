-----------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- This is project one of the Compiler Design class taught by Dr. Kramer
-- at Youngstown State University. This code takes input in the format
-- specified in the assignment documented and produces three C++ files
-- if the input is in the expected format.
--
-- This code is hereby released under the BSD-2 license. See LICENSE for more
-- information.
----------------------------------------------------------------------------
module Main where

import Control.Monad
import System.Environment (getArgs, getProgName)
import Text.Parser.Char

import ProjectOne.Extraction.CPlusPlus

-- | This defines a data type for representing the various regex rules.
-- Each constructor represents a rule. We derive an 'Eq' instance so that
-- Haskell can automatically compare two 'Regex' values without us having to
-- write a (laborious) Eq instance ourselves which pattern-matches on each
-- constructor and compares its parameters.
data RegexRule = Epsilon
               | Literal Char
               | Or RegexRule RegexRule
               | Then RegexRule RegexRule
               | Star RegexRule
               deriving (Eq)

-- | We don't derive Show above, because we can do something even cooler.
instance Show RegexRule where
  show Epsilon = "ε"
  show (Literal c) = [c]
  show (Or a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
  show (Then a b) = "(" ++ show a ++ show b ++ ")"
  show (Star a) = "(" ++ show a ++ ")*"

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

main :: IO ()
main = do
  arguments <- getArgs
  progname <- getProgName
  when (length arguments /= 2) $
    error $ "Usage: " ++ progname ++ " <input file> <output prefix>"
