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

import Data.Maybe (fromMaybe)
import Options.Applicative
import Text.Trifecta hiding (Parser)

import ProjectOne.Input.SpecLineParser

data OutputMode =
  Graphviz {
      inputFile :: String
    , nfa :: Bool
  }
  | Text {
      inputFile :: String
    , nfa :: Bool
  }
  | CPlusPlus {
      inputFile :: String
    , nfa :: Bool
  } deriving (Eq, Show, Ord)

subArgs :: (String -> Bool -> OutputMode) -> Parser OutputMode
subArgs f = f
  <$> strArgument
      ( metavar "RULESFILE"
     <> help "Regex rules file" )
  <*> switch
      ( long "nfa"
     <> short 'n'
     <> help "Use an NFA instead of a DFA" )

args :: Parser OutputMode
args = subparser
        (command "graphviz"
         (info (subArgs Graphviz)
          (progDesc "Output graphviz graphs of state transitions"))
      <> command "text"
         (info (subArgs Text)
          (progDesc "Output text representations of state transitions"))
      <> command "cplusplus"
         (info (subArgs CPlusPlus)
          (progDesc "Output C++ code based on the input specification")))

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Generate an NFA or DFA for a given input file"
     <> header "projectone - an implementation of CSCI 5807 project one" )

main' :: OutputMode -> IO ()
main' om = do
  case om of
   Graphviz i n -> do
     error "You've selected graphviz output!"
   Text i n -> do
     error "You've selected text output!"
   CPlusPlus i n -> do
     error "You've selected C++ output!"
  --inputLines <- fromSpecFile input
  --print inputLines
