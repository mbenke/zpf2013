{-# LANGUAGE Rank2Types #-}
module MyParsec3c.Prim where

type State = String
type ParseError = String

newtype Parser a = Parser {unParser :: forall b .
                 State
              -> (a -> State -> ParseError -> b) -- consumed ok
              -> (ParseError -> b)               -- consumed err
              -> (a -> State -> ParseError -> b) -- empty ok
              -> (ParseError -> b)               -- empty err
              -> b
             }
