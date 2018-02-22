module Lib
    ( Result(..)
    , pchar
    , run
    , andThen
    , (.>>.)
    , orElse
    , (<|>)
    , anyOf
    , parseLowercase
    , parseDigit
    , mapP) where

data Result a =
    Success a |
    Failure String
    deriving (Eq, Show)

data Parser t =
    Parser (String -> Result (t, String))

pchar :: Char -> Parser Char
pchar char = Parser f
    where
        f [] = Failure "No more input"
        f (x:xs) = if x == char
                   then Success (char, xs)
                   else Failure $ "Expecting " ++ show char ++ ". Got " ++ show x

run :: Parser t -> String -> Result (t, String)
run (Parser f) input = f input

andThen :: Parser t -> Parser t -> Parser (t, t)
andThen parser1 parser2 = Parser f
    where
        f input = case run parser1 input of
                      Failure s -> Failure s
                      Success (value1, remaining1) ->
                          case run parser2 remaining1 of
                              Failure s -> Failure s
                              Success (value2, remaining2) -> Success ((value1, value2), remaining2)

(.>>.) = andThen

orElse :: Parser t -> Parser t -> Parser t
orElse parser1 parser2 = Parser f
    where
        f input = case run parser1 input of
            s@(Success _) -> s
            Failure _ -> run parser2 input

(<|>) = orElse

choice :: [Parser t] -> Parser t
choice = foldr1 orElse

anyOf :: [Char] -> Parser Char
anyOf = choice . map pchar

parseLowercase :: Parser Char
parseLowercase = anyOf ['a'..'z']

parseDigit :: Parser Char
parseDigit = anyOf ['0'..'9']

mapP :: (a -> b) -> Parser a -> Parser b
mapP f parser = Parser innerF
    where
        innerF input = case run parser input of
            Failure s -> Failure s
            Success (value, remaining) -> Success (f value, remaining)

(<!>) = mapP

parseThreeDigitsAsStr :: () -> Parser String
parseThreeDigitsAsStr = mapP transformTuple tupleParser
    where
        tupleParser = parseDigit .>>. parseDigit .>>. parseDigit
        transformTuple ((c1, c2), c3) = [c1, c2, c3]
