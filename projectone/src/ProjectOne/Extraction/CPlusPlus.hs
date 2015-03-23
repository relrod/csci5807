-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.Extraction.CPlusPlus
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- This module deals specifically with code extraction out to C++. We implement
-- as much of the requirements as possible in Haskell and extract as little as
-- possible out to C++, as a rule.
----------------------------------------------------------------------------
module ProjectOne.Extraction.CPlusPlus (
  preamble
, postamble

-- * Header file declarations
, classDef

-- * Actual finite automata extraction
, outputEdges
) where

import qualified Data.Set as S
import Data.Time
import ProjectOne.DFA
import ProjectOne.NFA

preamble :: Bool -- ^ True if this is a header file, false if not
         -> UTCTime -- ^ Used for the "Generated: " line
         -> String
preamble h u =
  "// -------------------------------------------------------------------- //\n\
  \// This is an AUTOMATICALLY GENERATED file. It was generated as part of //\n\
  \// a project for Youngstown State University's Compiler Design class.   //\n\
  \//                                                                      //\n"
  ++ generatedLine ++
  "// -------------------------------------------------------------------- //\n"
  ++
  if h
  then
    "#ifndef PROJECT_ONE_INCLUDE\n\
    \#define PROJECT_ONE_INCLUDE\n"
  else ""
  where
    generated = "// Generated: " ++ show u
    spacesGeneratedLine = replicate (72 - length generated) ' '
    generatedLine = generated ++ spacesGeneratedLine ++ "//\n"
{-# INLINE preamble #-}

postamble :: Bool -- ^ True if this is a header file, false if not
          -> String
postamble h = if h
              then "#endif /* PROJECT_ONE_INCLUDE guard */"
              else ""
{-# INLINE postamble #-}

-- Header file declarations --

classDef :: String
classDef =
  "class LexicalAnalyzer {\n\
  \  public:\n\
  \    LexicalAnalyzer(istream &input = cin);\n\
  \    void start(void);\n\
  \    bool next(Token &t, string &lexeme) throw invalid_argument;\n\
  \};\n"
{-# INLINE classDef #-}

-- Actual output --

-- | Output the edge table for a 'DFA'.
outputEdges :: DFA Int -> String
outputEdges dfa =
  unlines . S.toList $ S.map f e
  where
    NFA _ e _ _ = getNFA dfa
    f (Edge a ch b) =
      "edge[" ++ show a ++ "][" ++ show ch ++ "] = " ++ show b ++ ";"
    f (Epsilon a b) = "epsilon[" ++ show a ++ "] = " ++ show b ++ ";"

-- | Output the list of accepting states for a 'DFA'.
outputAcceptingStates :: DFA Int -> String
outputAcceptingStates dfa =
  unlines . S.toList $ S.map f e
  where
    NFA _ e _ _ = getNFA dfa
    f (Edge a ch b) =
      "edge[" ++ show a ++ "][" ++ show ch ++ "] = " ++ show b ++ ";"
    f (Epsilon a b) = "epsilon[" ++ show a ++ "] = " ++ show b ++ ";"
