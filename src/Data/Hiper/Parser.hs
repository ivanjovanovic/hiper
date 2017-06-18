module Data.Hiper.Parser
       (
         parseEnvVal
       ) where

import Text.ParserCombinators.Parsec
import Data.Text

import Data.Hiper.Types.Internal

envValParser :: GenParser Char st Value
envValParser = numberEnvVal <|> stringEnvVal

numberEnvVal = do
  number <- many digit
  case number of
    "" -> fail ""
    _ -> return (Number (read number))

stringEnvVal = do
  result <- many1 anyChar
  return (String (pack result))

parseEnvVal :: String -> Either ParseError Value
parseEnvVal envVal = case envVal of
  "1" -> return (Bool True)
  "0" -> return (Bool False)
  "true" -> return (Bool True)
  "false" -> return (Bool False)
  _ ->  parse envValParser "" envVal
