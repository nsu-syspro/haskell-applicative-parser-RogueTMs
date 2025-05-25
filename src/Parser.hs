{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse (Parser p) s = p (Position 0 s)

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case parse p s of
  Parsed x _   -> Just x
  Failed _errs -> Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp ->
    case p inp of
      Parsed x inp' -> Parsed (f x) inp'
      Failed errs   -> Failed errs

instance Applicative Parser where
  pure x = Parser $ \inp -> Parsed x inp
  Parser pf <*> Parser pa = Parser $ \inp ->
    case pf inp of
      Parsed f inp' ->
        case pa inp' of
          Parsed x inp'' -> Parsed (f x) inp''
          Failed errs    -> Failed errs
      Failed errs -> Failed errs

instance Alternative Parser where
  empty = Parser $ \_ -> Failed []
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  Parser p1 <|> Parser p2 = Parser $ \inp ->
    case p1 inp of
      Parsed x inp' -> Parsed x inp'
      Failed e1     ->
        case p2 inp of
          Parsed y inp'' -> Parsed y inp''
          Failed e2      -> Failed (nub (e1 ++ e2))

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy test = Parser $ \(Position i str) ->
  case str of
    []      -> Failed [Position i EndOfInput]
    c : cs
      | test c   -> Parsed c (Position (i+1) cs)
      | otherwise -> Failed [Position i (Unexpected c)]
