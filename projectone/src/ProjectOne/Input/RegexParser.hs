-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.Input.RegexParser
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Here is where we actually parse the regex. We separate this out into its own
-- module entirely to prove that we are doing the parsing work ourself.
--
-- That said, we do make use of the \'trifecta\' library here and the
-- \'parsers\' abstraction on top of it. This is for a few reasons:
--
-- * We default to using Trifecta because it provides really nice error handling
-- for the case where we're given invalid input. It is fairly slow, but for our
-- purposes, it works very well.
--
-- * The project specification says that we are not to use any library to
-- *process* the regex. We are not doing that here, we are only using it to
-- parse the regex into types that we can (manually) process. That is, we parse
-- the regex and ensure it's valid (to the extent that our parser is correct)
-- and then we manually construct the NFA and transform it to a DFA, in other
-- modules.
--
-- * As for why we are using the \'parsers\' abstraction, there is a chance
-- that, even though we're not doing any processing on the regex, that this
-- is borderline within the project specification. By using the \'parsers\'
-- abstraction, even if we have to write our own parser combinator library,
-- none of the code in this file will ever have to change, so long as our
-- custom parser combinator library can have instances that \'parsers\' exports.
-- So we effectively safeguard ourselves from having to change all this code
-- later, in the event that we are told that \'trifecta\' is not a valid option.
----------------------------------------------------------------------------
module ProjectOne.Input.RegexParser where

import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import ProjectOne.RegexRule
import Text.Parser.Char
import Text.Parser.Combinators

-- | Characters which require escaping.
specialChars :: String
specialChars = "+?*()[]\\"

parseRegex :: (Monad m, CharParsing m) => m RegexRule
parseRegex = do
  error ""

-- TODO: In reality, there can be more than a literal here, but I haven't
-- written a generalized token parser for this yet, so we specialize it for now
-- so that other functions can check.
token :: (Monad m, CharParsing m) => m RegexRule
token = literal

literal :: (Monad m, CharParsing m) => m RegexRule
literal = Literal <$> (noneOf specialChars <|> (char '\\' *> anyChar))

-- | Represents some optional match. Another way of saying this is @ε|α@ where
-- ε is the empty string and α is some token.
optional :: (Monad m, CharParsing m) => m RegexRule
optional = Or Epsilon <$> (token <* char '?')

star :: (Monad m, CharParsing m) => m RegexRule
star = Star <$> (token <* char '*')

plus :: (Monad m, CharParsing m) => m RegexRule
plus = ap Then Star <$> (token <* char '+')
