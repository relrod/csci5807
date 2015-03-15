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
) where

import Data.Time

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
