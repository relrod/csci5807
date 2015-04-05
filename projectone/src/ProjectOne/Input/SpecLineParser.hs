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
, fromSpecFile
) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Monoid (mempty)
import Data.Traversable
import ProjectOne.Input.RegexParser
import ProjectOne.RegexRule hiding (Class)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta

data SpecLine = Class { className :: String
                      , classSet  :: RegexRule
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
  decl <- choice [comment, classDecl, tokenDecl, ignoreDecl]
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
      classMembers <- manyTill (choice [try charRange, return <$> anyChar]) (try (char ']'))
      return $ Class identifier (foldr (Or . Literal) Epsilon (join classMembers))

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
{-# INLINE specLines #-}

-- | This is a last-minute hack because I can't figure out how to easily get
-- my 'regexParser' function to stop consuming newlines.
--
-- What we do instead here is read in all input, split it into lines, and send
-- each line to the parser. Then we take advantage of the fact that Trifecta\'s
-- 'Result' is an 'Applicative' functor and use 'sequenceA' to go from
--   @IO [Result SpecLine]@ back to @IO (Result [SpecLine])@.
--
-- Unfortunately this hack is really bad -- it causes us to lose context.
-- When calling this, if you send us an invalid spec file, our error messages
-- no longer contain valid line numbers. This is something that should be fixed,
-- but I was tired of being blocked on this issue and I came up with this
-- temporary workaround.
--
-- Nothing here actually breaks our project specification (which leaves the
-- action to take on erroneous input files undefined), so this does not impact
-- the _correctness_ of the project. It does, however, make our use of Trifecta
-- slightly less valuable and should be fixed.
fromSpecFile :: FilePath -> IO (Result [SpecLine])
fromSpecFile fn = do
  fLines <- B.lines <$> B.readFile fn
  -- Filter out blank lines and comments
  let fLines' =
        filter (\x -> not (B.pack "//" `B.isPrefixOf` x
                           || x == B.pack "")) fLines
  return . sequenceA $ parseByteString parseSpecLine mempty <$>fLines'
{-# INLINE fromSpecFile #-}
