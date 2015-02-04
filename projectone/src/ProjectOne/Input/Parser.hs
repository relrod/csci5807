-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.Input.Parser
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Here is where we parse the input specification into forms that we can
-- manipulate internally.
----------------------------------------------------------------------------
module ProjectOne.Input.Parser (
  SpecLine (..)
, parseSpecLine
, specLines
) where

import qualified Data.Set as S
import Text.Parser.Char
import Text.Parser.Combinators

data SpecLine = Class { className :: String
                      , classSet  :: S.Set Char
                      }
              | Token { tokenName  :: String
                      , tokenRegex :: String
                        -- ^ By specification, we cannot do anything else here.
                      }
              | Ignore { ignoreRegex :: String
                        -- ^ By specification, we cannot do anything else here.
                       }
              deriving (Eq, Ord, Show)

-- | We use the @parsers@ abstraction library atop the @trifecta@ parser
-- combinator library but __only__ for parsing input lines. Notably,
-- page 3 of the project specification says to not touch the regex with any
-- any library, so we make a point here of only doing sanity checks on it and
-- not actually treating it as a regex in any manner. The sanity checks we do
-- are as follows:
--
--    * (None yet)
parseSpecLine :: (Monad m, CharParsing m) => m SpecLine
parseSpecLine = do
  choice [classDecl, tokenDecl, ignoreDecl]
  where
    classDecl = do
      _ <- string "class"
      _ <- some space
      -- TODO: Check to ensure this is a valid C++ identifier.
      identifier <- many alphaNum
      _ <- some space
      _ <- char '['
      setMembers <- manyTill anyChar (try (char ']'))
      _ <- newline
      return $ Class identifier (S.fromList setMembers)

    tokenDecl = do
      _ <- string "token"
      _ <- some space
      -- TODO: Check to ensure this is a valid C++ identifier.
      identifier <- manyTill anyChar (try (some space))
      regex <- manyTill anyChar (try newline)
      return $ Token identifier regex

    ignoreDecl = do
      _ <- string "ignore"
      _ <- some space
      regex <- manyTill anyChar (try newline)
      return $ Ignore regex

specLines :: (Monad m, CharParsing m) => m [SpecLine]
specLines = manyTill parseSpecLine (try eof)
