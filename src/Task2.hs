{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import Control.Applicative (some, (<|>))
import ParserCombinators (string, char)

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = dotFormat <|> hyphenFormat <|> usFormat
  where
    -- helpers
    toInt   = foldl (\a c -> a * 10 + fromEnum c - fromEnum '0') 0
    digit   = satisfy (\c -> c >= '0' && c <= '9')
    nonZero = satisfy (\c -> c >= '1' && c <= '9')

    -- two‐digit day
    day2 =
          Day 30        <$ string "30"
      <|> Day 31        <$ string "31"
      <|> (\_ d -> Day (fromEnum d - fromEnum '0')) <$> char '0' <*> nonZero
      <|> (\_ d -> Day (10 + fromEnum d - fromEnum '0')) <$> char '1' <*> digit
      <|> (\_ d -> Day (20 + fromEnum d - fromEnum '0')) <$> char '2' <*> digit

    -- two‐digit month
    month2 =
          Month 10      <$ string "10"
      <|> Month 11      <$ string "11"
      <|> Month 12      <$ string "12"
      <|> (\_ d -> Month (fromEnum d - fromEnum '0')) <$> char '0' <*> nonZero

    -- unbounded‐length year
    yearP = Year . toInt <$> some digit

    -- three–letter month name
    monthName =
          Month 1  <$ string "Jan"
      <|> Month 2  <$ string "Feb"
      <|> Month 3  <$ string "Mar"
      <|> Month 4  <$ string "Apr"
      <|> Month 5  <$ string "May"
      <|> Month 6  <$ string "Jun"
      <|> Month 7  <$ string "Jul"
      <|> Month 8  <$ string "Aug"
      <|> Month 9  <$ string "Sep"
      <|> Month 10 <$ string "Oct"
      <|> Month 11 <$ string "Nov"
      <|> Month 12 <$ string "Dec"

    -- US‐style day, no leading zero
    usDay =
          Day 30        <$ string "30"
      <|> Day 31        <$ string "31"
      <|> (\_ d -> Day (10 + fromEnum d - fromEnum '0')) <$> char '1' <*> digit
      <|> (\_ d -> Day (20 + fromEnum d - fromEnum '0')) <$> char '2' <*> digit
      <|> Day . (\c -> fromEnum c - fromEnum '0') <$> nonZero

    -- the three formats
    dotFormat    = pure Date
                   <*> (day2   <* char '.')
                   <*> (month2 <* char '.')
                   <*> yearP

    hyphenFormat = pure Date
                   <*> (day2   <* char '-')
                   <*> (month2 <* char '-')
                   <*> yearP

    usFormat     = pure (\m d y -> Date d m y)
                   <*> monthName
                   <*> (char ' ' *> usDay)
                   <*> (char ' ' *> yearP)
