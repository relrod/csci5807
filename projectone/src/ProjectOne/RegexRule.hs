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
, ε
, specialChars
, match
) where

import Control.Applicative
import qualified Data.List.NonEmpty as NEL

-- | Characters which require escaping.
specialChars :: String
specialChars = "+?*()[]\\"

-- | This defines a data type for representing the various regex rules.
-- Each constructor represents a rule. We derive an 'Eq' instance so that
-- Haskell can automatically compare two 'Regex' values without us having to
-- write a (laborious) Eq instance ourselves which pattern-matches on each
-- constructor and compares its parameters.
data RegexRule = Epsilon
               | Literal Char
               | Or RegexRule RegexRule
               | Concat RegexRule RegexRule
               | Star RegexRule
               deriving (Eq)

ε :: RegexRule
ε = Epsilon

-- | We don't derive Show above, because we can do something even cooler.
-- Using this, we can roughly recover the initial regex, as parsed.
instance Show RegexRule where
  show Epsilon = "ε"
  show (Literal c) = if c `elem` specialChars
                     then "\\" ++ [c]
                     else [c]
  show (Or a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
  show (Concat a b) = "(" ++ show a ++ show b ++ ")"
  show (Star a) = "(" ++ show a ++ ")*"

-- | Given a 'RegexRule', attempt to match a string against it.
match :: RegexRule -> String -> Bool
match Epsilon x      = x == ""
match (Literal c) x  = x == [c]
match (Or a b) x   = match a x || match b x
match (Concat a b) x =
  or (concatMap (\(a', b') ->
                  [match a a' && match b b']) (NEL.toList $ allSplits x))
match s@(Star a) x =
  match ε x || or (concatMap (\(a', b') ->
                               [match a a' && match s b']) (NEL.tail . allSplits $ x))

allSplits :: [a] -> NEL.NonEmpty ([a], [a])
allSplits [] = NEL.fromList [([], [])]
allSplits xs = flip NEL.splitAt (NEL.fromList xs) <$> NEL.fromList [0..length xs]
