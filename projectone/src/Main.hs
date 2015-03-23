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
import Options.Applicative
import System.Environment (getArgs, getProgName)
import Text.Trifecta hiding (Parser)

import ProjectOne.Extraction.CPlusPlus
import ProjectOne.Extraction.TextTable
import ProjectOne.Input.SpecLineParser
import ProjectOne.RegexRule

data OutputMode = CPlusPlus | TextTable deriving (Eq, Show, Ord)

data Arguments = Arguments {
    inputFile :: String
  , nfa :: Bool
  --, mode :: OutputMode
  } deriving (Eq, Show, Ord)

args :: Parser Arguments
args = Arguments
  <$> strOption
      ( long "hello"
     <> metavar "TARGET"
     <> help "Target for the greeting" )
  <*> switch
      ( long "nfa"
     <> short 'n'
     <> help "Use an NFA instead of a DFA" )
  -- TODO: OutputMode

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Generate an NFA or DFA for a given input file"
     <> header "projectone - an implementation of CSCI 5807 project one" )

main' :: Arguments -> IO ()
main' (Arguments input shouldUseNfa) = do
  inputLines <- parseFromFile specLines input
  case inputLines of
   Just lines' -> print lines'
   Nothing     -> error "Invalid file format given."
