{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

-- | Lower-level flatparse parsers
module DotParse.FlatParse
  ( Error(..),
    prettyError,
    keyword,
    keyword',
    symbol,
    symbol',
    ws,
    token,
    ident,
    cut,
    cut',
    testParser,
    runParser_,
    int,
    double,
    signed,
    quoted,
    htmlLike,
    sepP,
    wrapSquareP,
    wrapSquarePrint,
    wrapCurlyP,
    wrapCurlyPrint,
    wrapQuotePrint,
    pointP,
    curveP,
    rectP,
    boolP,
    nonEmptyP,
  ) where

import FlatParse.Basic hiding (cut, lines)
import DotParse.FlatParse.TH hiding (merge)
import Data.Char hiding (isDigit)
import Data.ByteString hiding (zip, zipWith, putStrLn, map, length, head, empty)
import Data.Bool
import Prelude hiding (replicate)
import qualified Data.ByteString.Char8 as B
import NumHask.Space
import Data.List.NonEmpty

-- $setup
-- >>> import DotParse
-- >>> import FlatParse.Basic

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser Error a -> ByteString -> IO ()
testParser p b =
  case runParser p b of
    Err e  -> B.putStrLn $ prettyError b e
    OK a _ -> print a
    Fail   -> B.putStrLn "uncaught parse error"

-- | run a Parser, erroring on leftovers, Fail or Err
runParser_ :: Parser Error a -> ByteString -> a
runParser_ p b = case runParser p b of
  OK r "" -> r
  OK _ x -> error $ unpackUTF8 $ "leftovers: " <> x
  Fail -> error "Fail"
  Err e -> error $ unpackUTF8 $ prettyError b e

-- * parsing
digit :: Parser Error Int
digit = (\c -> ord c - ord '0') <$> satisfyASCII isDigit

int :: Parser Error Int
int = token do
  (place, n) <- chainr (\n (!place, !acc) -> (place*10,acc+place*n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser Error (Int, Int)
digits = chainr (\n (!place, !acc) -> (place*10,acc+place*n)) digit (pure (1, 0))

-- |
-- >>> runParser double "1.234x"
-- OK 1.234 "x"
--
-- >>> runParser double "."
-- Fail
--
-- >>> runParser double "123"
-- OK 123.0 ""
--
-- >>> runParser double ".123"
-- OK 0.123 ""
--
-- >>> runParser double "123."
-- OK 123.0 ""
double :: Parser Error Double
double = token do
  (placel, nl) <- digits
  optioned ($(char '.') *> digits)
    (\(placer, nr) ->
       case (placel, placer) of
         (1,1) -> empty
         _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer)
    (case placel of
      1 -> empty
      _ -> pure $ fromIntegral nl)

-- |
-- >>> runParser (signed double) "-1.234x"
-- OK (-1.234) "x"
signed :: Num b => Parser e b -> Parser e b
signed p = optioned $(char '-') (const (((-1) *) <$> p)) p

-- | Looks ahead for a "/"" that may be in the quoted string.
-- >>> runParser quoted (packUTF8 "\"hello\"")
-- OK "hello" ""
--
-- >>> runParser quoted (packUTF8 "\"hello/\"\"")
-- OK "hello\"" ""
--
quoted :: Parser Error String
quoted =
  $(symbol "\"") *> many unquoteQuote <* $(symbol' "\"")

unquoteQuote :: Parser Error Char
unquoteQuote = do
  next <- satisfy (/= '"')
  case next of
    '/' -> branch (lookahead $(char '"')) ('"' <$ $(char '"')) (pure '/')
    x -> pure x

-- | optional separators
sepP :: Parser e ()
sepP = token
  $(switch [| case _ of
    ";"  -> pure ()
    "," -> pure ()
            |])

-- | parse wrapping square brackets
wrapSquareP :: Parser Error a -> Parser Error a
wrapSquareP p =
  $(symbol "[") *> p <* $(symbol' "]")

-- | print wrapping square brackets
wrapSquarePrint :: ByteString -> ByteString
wrapSquarePrint b = "[" <> b <> "]"

-- | print wrapping quotes
wrapQuotePrint :: ByteString -> ByteString
wrapQuotePrint b = "\"" <> b <> "\""

-- | parse wrapping square brackets
wrapCurlyP :: Parser Error a -> Parser Error a
wrapCurlyP p = $(symbol "{") *> p <* $(symbol' "}")

-- | print wrapping curly brackets
wrapCurlyPrint :: ByteString -> ByteString
wrapCurlyPrint b = "{" <> b <> "}"

-- | comma separated Point
pointP :: Parser Error (Point Double)
pointP = token $ Point <$> double <*> ($(symbol ",") *> double)

-- | dot curve representation
--
-- Assumes the end specification
curveP :: Parser Error [Point Double]
curveP = $(symbol "e,") *> many pointP

-- | comma separated rectangle or bounding box
rectP :: Parser Error (Rect Double)
rectP = token $ do
  x <- double
  _ <- $(symbol ",")
  y <- double
  _ <- $(symbol ",")
  z <- double
  _ <- $(symbol ",")
  w <- double
  pure $ Rect x z y w

-- | true | false
boolP :: Parser Error Bool
boolP =
  (True <$ $(symbol "true")) <|>
  (False <$ $(symbol "false"))

-- | NonEmpty version of many
nonEmptyP :: Parser e a -> Parser e () -> Parser e (NonEmpty a)
nonEmptyP p sep = token $ do
          s <- p
          xs <- many (optional sep *> p)
          pure (s :| xs)