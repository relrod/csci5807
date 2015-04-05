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

data OutputMode = CPlusPlus | TextTable | Graphviz deriving (Eq, Show, Ord)

data Arguments = Arguments {
    inputFile :: String
  , mode :: String
  , nfa :: Bool
  } deriving (Eq, Show, Ord)

strToOutputMode :: String -> Maybe OutputMode
strToOutputMode "cplusplus" = Just CPlusPlus
strToOutputMode "text"      = Just TextTable
strToOutputMode "graphviz"  = Just Graphviz
strToOutputMode _           = Nothing

args :: Parser Arguments
args = Arguments
  <$> strArgument
      ( metavar "RULESFILE"
     <> help "Regex rules file" )
  <*> strOption
      ( metavar "OUTPUTFORMAT"
     <> help "Output format (cplusplus, text, graphviz)"
     <> value "text" )
  <*> switch
      ( long "nfa"
     <> short 'n'
     <> help "Use an NFA instead of a DFA" )

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Generate an NFA or DFA for a given input file"
     <> header "projectone - an implementation of CSCI 5807 project one" )

main' :: Arguments -> IO ()
main' (Arguments input mode' _) = do
  let _mode'' = fromMaybe (error "Invalid mode specified") (strToOutputMode mode')
  inputLines <- fromSpecFile input
  print inputLines
