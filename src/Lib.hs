module Lib
    ( Result(..), pchar, run, andThen, (.>>.), orElse, (<|>) ) where

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
