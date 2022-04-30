{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- | Abstract Grammar from:
-- http://www.graphviz.org/doc/info/lang.html

module Chart.Dot.TH where

import Language.Haskell.TH
import FlatParse.Basic
import Data.Functor
import Data.ByteString hiding (reverse)

isKeyword :: Span -> Parser e ()
isKeyword span' = inSpan span' do
  $(switch [| case _ of
      "strict" -> pure ()
      "graph" -> pure ()
      "digraph" -> pure ()
      "node"  -> pure ()
      "edge"  -> pure ()
      "subgraph" -> pure ()
      "n"  -> pure ()
      "ne"  -> pure ()
      "e"  -> pure ()
      "se"  -> pure ()
      "s"  -> pure ()
      "sw"  -> pure ()
      "w"  -> pure ()
      "nw"  -> pure ()
      "c"  -> pure ()
      "_"  -> pure ()
      "=" -> pure ()
      "--" -> pure ()
      "->" -> pure ()
       |])
  eof

-- | Consume whitespace.
ws :: Parser e ()
ws = $(switch [| case _ of
  " "  -> ws
  "\n" -> ws
  "\t" -> ws
  "\r" -> ws
  ";" -> ws

  "//" -> lineComment
  "/*" -> multilineComment
  _    -> pure () |])

-- | Parse a line comment.
lineComment :: Parser e ()
lineComment =
  optioned anyWord8
    (\case 10 -> ws
           _  -> lineComment)
    (pure ())

-- | Parse a potentially nested multiline comment.
multilineComment :: Parser e ()
multilineComment = go (1 :: Int) where
  go 0 = ws
  go n = $(switch [| case _ of
    "*/" -> go (n - 1)
    "/*" -> go (n + 1)
    _    -> branch anyWord8 (go n) (pure ()) |])

-- | Parse a HTML-Like string by counting the angle brackets
htmlLike :: Parser e String
htmlLike = ws *> $(char '<') *> go (1 :: Int) "<" where
  go 0 acc = ws $> reverse acc
  go n acc =
    ($(char '>') *> go (n - 1) ('>':acc)) <|>
    ($(char '<') *> go (n + 1) ('<':acc)) <|>
    (anyChar >>= (\c -> go n (c:acc)))

-- | Consume whitespace after running a parser.
token :: Parser e a -> Parser e a
token p = p <* ws
{-# inline token #-}

isValidStartChar :: Char -> Bool
isValidStartChar c =
  ('A' <= c && c <= 'Z') ||
  ('a' <= c && c <= 'z') ||
  ('\200' <= c && c <= '\377') ||
  (c == '_')

isValidChar :: Char -> Bool
isValidChar c = isValidStartChar c || isDigit c

-- | Read a starting character of an identifier.
identStartChar :: Parser e Char
identStartChar = satisfy isValidStartChar

-- | Read a non-starting character of an identifier.
identChar :: Parser e Char
identChar = satisfy isValidChar

-- | Parse a non-keyword string.
symbol :: String -> Q Exp
symbol str = [| token $(string str) |]

-- | Parse a keyword string.
keyword :: String -> Q Exp
keyword str = [| token ($(string str) `notFollowedBy` identChar) |]

-- | Parse an identifier. This parser uses `isKeyword` to check that an identifier is not a
--   keyword.
ident :: Parser e ByteString
ident = token $ byteStringOf $
  spanned (identStartChar *> many_ identChar) (\_ span -> fails (isKeyword span))
