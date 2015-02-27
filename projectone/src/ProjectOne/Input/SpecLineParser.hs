-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.Input.SpecLineParser
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Here is where we parse the input specification into forms that we can
-- manipulate internally.
----------------------------------------------------------------------------
module ProjectOne.Input.SpecLineParser (
  SpecLine (..)
, parseSpecLine
, specLines
) where

import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import ProjectOne.Input.RegexParser
import ProjectOne.RegexRule
import Text.Parser.Char
import Text.Parser.Combinators

data SpecLine = Class { className :: String
                      , classSet  :: S.Set Char
                      }
              | Token { tokenName  :: String
                      , tokenRegex :: RegexRule
                      }
              | Ignore { ignoreRegex :: RegexRule
                       }
              | Comment String
              deriving (Eq, Show)

-- | We use the @parsers@ abstraction library atop the @trifecta@ parser
-- combinator library but __only__ for parsing input lines. Notably,
-- page 3 of the project specification says to not touch the regex with any
-- any regex library. We are not doing that, but we are using a parsing library
-- to parse it into Haskell types that we can work with. All of the
-- NFA/DFA/transformations are done manually (see @ProjectOne.NFA@, etc.).
parseSpecLine :: (Monad m, CharParsing m) => m SpecLine
parseSpecLine = do
  skipMany space
  decl <- choice [classDecl, tokenDecl, ignoreDecl, comment]
  skipMany space
  return decl
  where
    charRange :: (Monad m, CharParsing m) => m String
    charRange = liftA2 enumFromTo (anyChar <* char '-') anyChar

    classDecl = do
      _ <- string "class"
      _ <- some space
      -- TODO: Check to ensure this is a valid C++ identifier.
      identifier <- manyTill anyChar (try (some space))
      _ <- char '['
      setMembers <- manyTill (choice [try charRange, return <$> anyChar]) (try (char ']'))
      _ <- newline
      return $ Class identifier (S.fromList . join $ setMembers)

    tokenDecl = do
      _ <- string "token"
      _ <- some space
      -- TODO: Check to ensure this is a valid C++ identifier.
      identifier <- manyTill anyChar (try (some space))
      regex <- parseRegex
      return $ Token identifier regex

    ignoreDecl = do
      _ <- string "ignore"
      _ <- some space
      regex <- parseRegex
      return $ Ignore regex

    comment = do
      _ <- string "//"
      _ <- many space
      cmt <- manyTill anyChar (try newline)
      return $ Comment cmt

specLines :: (Monad m, CharParsing m) => m [SpecLine]
specLines = manyTill parseSpecLine (try eof)
