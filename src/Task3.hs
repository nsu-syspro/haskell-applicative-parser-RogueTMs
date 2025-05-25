{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import Control.Applicative (some, many, (<|>), optional)
import Data.Char (toLower, isDigit)
import Data.List (intercalate)
import ParserCombinators (char, string, spaces)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = spaces *> value <* spaces

value :: Parser JValue
value =
      jObject
  <|> jArray
  <|> JString <$> jsonString
  <|> JNumber <$> number
  <|> JBool True  <$ string "true"
  <|> JBool False <$ string "false"
  <|> JNull       <$ string "null"

jsonString :: Parser String
jsonString = char '"' *> (concat <$> many charContent) <* char '"'

charContent :: Parser String
charContent =
      unicodeEscape
  <|> (\c -> ['\\',c]) <$> (char '\\' *> satisfy (\c -> c `elem` "\"\\/bfnrt"))
  <|> (:[]) <$> satisfy (\c -> c /= '"' && c /= '\\')

unicodeEscape :: Parser String
unicodeEscape =
  (\p d1 d2 d3 d4 -> p ++ [d1,d2,d3,d4])
    <$> string "\\u"
    <*> satisfy isHex
    <*> satisfy isHex
    <*> satisfy isHex
    <*> satisfy isHex

isHex :: Char -> Bool
isHex c =
  (c >= '0' && c <= '9')
  || (c >= 'a' && c <= 'f')
  || (c >= 'A' && c <= 'F')

number :: Parser Double
number =
  (\s ip f ex -> read (s ++ ip ++ maybe "" id f ++ maybe "" id ex))
    <$> ((\c -> [c]) <$> char '-' <|> pure "")
    <*> ((string "0") <|> ((:) <$> satisfy (\c -> c >= '1' && c <= '9') <*> many (satisfy isDigit)))
    <*> optional ((:) <$> char '.' <*> some (satisfy isDigit))
    <*> optional
          ((\e m ds -> e : maybe "" (:[]) m ++ ds)
             <$> satisfy (\c -> c == 'e' || c == 'E')
             <*> optional (char '+' <|> char '-')
             <*> some (satisfy isDigit))

jArray :: Parser JValue
jArray = JArray <$> (char '[' *> spaces *> elements <* spaces <* char ']')
  where
    elements =
         (:) <$> (spaces *> value <* spaces)
             <*> many (spaces *> char ',' *> spaces *> value <* spaces)
     <|> pure []

jObject :: Parser JValue
jObject = JObject <$> (char '{' *> spaces *> members <* spaces <* char '}')
  where
    members =
         (:) <$> member
             <*> many (spaces *> char ',' *> spaces *> member)
     <|> pure []

member :: Parser (String, JValue)
member =
  (,) <$> jsonString <* spaces <* char ':' <*> (spaces *> value)

render :: JValue -> String
render = concatMap readable . renderTokens
  where
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
