{-# OPTIONS_GHC -fwarn-missing-signatures #-}

module Lib
  ( Result(..)
  , pchar
  , run
  , choice
  , andThen
  , orElse
  , anyOf
  , parseLowercase
  , parseDigit
  , mapP
  , parseThreeDigitsAsStr
  , parseThreeDigitsAsInt
  , sequenceP
  , many
  ) where

data Result a
  = Success a
  | Failure String
  deriving (Eq, Show)

newtype Parser t =
  Parser (String -> Result (t, String))

pchar :: Char -> Parser Char
pchar char = Parser f
  where
    f [] = Failure "No more input"
    f (x:xs) =
      if x == char
        then Success (char, xs)
        else Failure $ "Expecting " ++ show char ++ ". Got " ++ show x

run :: Parser t -> String -> Result (t, String)
run (Parser f) = f

andThen :: Parser a -> Parser b -> Parser (a, b)
andThen parser1 parser2 = Parser f
  where
    f input =
      case run parser1 input of
        Failure s -> Failure s
        Success (value1, remaining1) ->
          case run parser2 remaining1 of
            Failure s -> Failure s
            Success (value2, remaining2) ->
              Success ((value1, value2), remaining2)

orElse :: Parser t -> Parser t -> Parser t
orElse parser1 parser2 = Parser f
  where
    f input =
      case run parser1 input of
        s@(Success _) -> s
        Failure _ -> run parser2 input

choice :: [Parser t] -> Parser t
choice = foldr1 orElse

anyOf :: [Char] -> Parser Char
anyOf = choice . map pchar

parseLowercase :: Parser Char
parseLowercase = anyOf ['a' .. 'z']

parseDigit :: Parser Char
parseDigit = anyOf ['0' .. '9']

mapP :: (a -> b) -> Parser a -> Parser b
mapP f parser = Parser innerF
  where
    innerF input =
      case run parser input of
        Failure s -> Failure s
        Success (value, remaining) -> Success (f value, remaining)

parseThreeDigitsAsStr :: Parser String
parseThreeDigitsAsStr = mapP transformTuple tupleParser
  where
    tupleParser = parseDigit `andThen` parseDigit `andThen` parseDigit
    transformTuple ((c1, c2), c3) = [c1, c2, c3]

parseThreeDigitsAsInt :: Parser Integer
parseThreeDigitsAsInt = mapP read parseThreeDigitsAsStr

returnP :: a -> Parser a
returnP x = Parser f
  where
    f input = Success (x, input)

applyP :: Parser (a -> b) -> Parser a -> Parser b
applyP fP xP = mapP lambda parsers
  where
    parsers = fP `andThen` xP
    lambda (f, x) = f x


lift2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f aP bP = returnP f `applyP` aP `applyP` bP

addP :: Parser Integer -> Parser Integer -> Parser Integer
addP = lift2 (+)

sequenceP :: [Parser a] -> Parser [a]
sequenceP [] = returnP []
sequenceP (head:tail) = consP head (sequenceP tail)
  where
    consP = lift2 (:)

pstring :: String -> Parser String
pstring str = sequenceP $ map pchar str

many :: Parser a -> Parser [a]
many parser = Parser innerFn
  where
    innerFn input = Success (parseZeroOrMore parser input)
    parseZeroOrMore :: Parser a -> String -> ([a], String)
    parseZeroOrMore parser input = case run parser input of
        Failure e -> ([], input)
        Success (firstValue, inputAfterFirstParse) ->
          (values, remainingInput)
            where
              (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterFirstParse
              values = firstValue : subsequentValues


