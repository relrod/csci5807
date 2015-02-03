-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.RegexRule
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- This module deals with our internal representation of a regex. Once we are
-- given a regex, we parse it into this representation to maniuplate it
-- internally.
----------------------------------------------------------------------------
module ProjectOne.RegexRule (
  RegexRule (..)
) where

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
-- Using this, we can roughly recover the initial regex, as parsed.
instance Show RegexRule where
  show Epsilon = "ε"
  show (Literal c) = [c]
  show (Or a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
  show (Then a b) = "(" ++ show a ++ show b ++ ")"
  show (Star a) = "(" ++ show a ++ ")*"
