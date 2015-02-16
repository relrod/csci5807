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
import Text.Trifecta

import ProjectOne.Extraction.CPlusPlus
import ProjectOne.Input.SpecLineParser
import ProjectOne.RegexRule

main :: IO ()
main = do
  arguments <- getArgs
  progname <- getProgName
  when (length arguments /= 2) $
    error $ "Usage: " ++ progname ++ " <input file> <output prefix>"
  let (inFile:outFiles:_) = arguments
  inputLines <- parseFromFile specLines inFile
  case inputLines of
   Just lines' -> print lines'
   Nothing     -> error "Invalid file format given."
