{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Parser
    ( consume
    , peek
    , eof
    , satisfy
    , char
    , string
    , sepBy
    , between
    , option
    , skipSpaces
    , word
    , natural
    , list
    , valueP
    , recordP
    , keyValueP
    )
where

import Control.Applicative
    ( Alternative (..)
    , Applicative (..)
    , many
    , some
    )
import Control.Monad (MonadPlus (..))
import Data.Functor (($>))
import qualified Data.Map.Lazy as Map
import Parser.Type (Parser (..), ParserS)
import Value (Record (..), Value (..))

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser p) = Parser $ \input -> do
        (input', x) <- p input
        Just (input', f x)

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure x = Parser $ \input -> Just (input, x)
    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    Parser p1 <*> Parser p2 = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    Parser p >>= f = Parser $ \input -> do
        (input', a) <- p input
        runParser (f a) input'

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ const Nothing
    (<|>) :: Parser s a -> Parser s a -> Parser s a
    Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input

instance (Semigroup a) => Semigroup (Parser s a) where
    (<>) :: Parser s a -> Parser s a -> Parser s a
    (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (Parser s a) where
    mempty :: Parser s a
    mempty = pure mempty

instance MonadFail (Parser s) where
    fail :: String -> Parser s a
    fail _ = empty -- how to avoid this forgetting the error message?

instance MonadPlus (Parser s) where
    mzero :: Parser s a
    mzero = empty
    mplus :: Parser s a -> Parser s a -> Parser s a
    mplus = (<|>)

-- take next token from input
consume :: Parser [a] a
consume = Parser $ \case
    [] -> Nothing
    (x : xs) -> Just (xs, x)

-- observe next token from input without consuming it
peek :: Parser [a] a
peek = Parser $ \case
    [] -> Nothing
    (x : xs) -> Just (x : xs, x)

-- match end of input
eof :: Parser [a] ()
eof = Parser $ \case
    [] -> Just ([], ())
    _ -> Nothing

-- match next token of input if it satisfies the predicate
satisfy :: (a -> Bool) -> Parser [a] a
satisfy f = do
    x <- consume
    if f x then pure x else empty

-- match next token of input if it is equal to the given token
char :: (Eq a) => a -> Parser [a] a
char c = satisfy (== c)

-- match a string of tokens
string :: (Eq a) => [a] -> Parser [a] [a]
string = traverse char

-- parse a list of values separated by a given separator
sepBy :: Parser s a -> Parser s b -> Parser s [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

-- parse a value enclosed by two ignored parsers
between :: Parser s a -> Parser s b -> Parser s c -> Parser s c
between open close p = open *> p <* close

-- parse a value or use a default value if the parser fails
option :: a -> Parser s a -> Parser s a
option x p = p <|> pure x

-- parse and skip zero or more space characters
skipSpaces :: ParserS ()
skipSpaces = many (char ' ') $> ()

-- parse a word as a string of lowercase letters
word :: ParserS String
word = some $ satisfy (`elem` (['a' .. 'z'] :: String))

natural :: ParserS Int
natural = read <$> some (satisfy (`elem` (['0' .. '9'] :: String)))

-- parse an haskell-style list of values
list :: ParserS a -> ParserS [a]
list p = between
    do char '[' >> skipSpaces
    do char ']' >> skipSpaces
    do p `sepBy` (char ',' >> skipSpaces)

valueP :: ParserS Value
valueP =
    VInt <$> natural
        <|> VString <$> word
        <|> VList <$> list valueP
        <|> VRecord <$> recordP

recordP :: ParserS Record
recordP =
    Record <$> between
        do char '{' >> skipSpaces
        do char '}' >> skipSpaces
        do
            r <- Map.fromList <$> sepBy keyValueP (char ',' *> skipSpaces)
            r <$ skipSpaces

keyValueP :: ParserS (String, Value)
keyValueP = do
    key <- word <* skipSpaces
    char ':' >> skipSpaces
    value <- valueP <* skipSpaces
    pure (key, value)
