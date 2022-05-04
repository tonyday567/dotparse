{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Abstract Grammar from:
-- http://www.graphviz.org/doc/info/lang.html

module Dot.TH where

import Language.Haskell.TH
import FlatParse.Basic
import Data.Functor
import Data.ByteString hiding (length, head, reverse)
import qualified Data.ByteString.Char8 as B

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
       |])
  eof

-- | Consume whitespace.

ws :: Parser e ()
ws = $(switch [| case _ of
  -- order matters
  " "  -> ws
  "\\\n" -> ws
  "\\\\\\n" -> ws
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

-- | Parser a non-keyword string, throw precise error on failure.
symbol' :: String -> Q Exp
symbol' str = [| $(symbol str) `cut'` packUTF8 str |]

-- | Parse a keyword string, throw precise error on failure.
keyword' :: String -> Q Exp
keyword' str = [| $(keyword str) `cut'` packUTF8 str |]

-- | Parse an identifier. This parser uses `isKeyword` to check that an identifier is not a
--   keyword.
ident :: Parser e ByteString
ident = token $ byteStringOf $
--   spanned (identStartChar *> many_ identChar) (\_ span -> fails (isKeyword span))
  identStartChar *> many_ identChar

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser Error ByteString
ident' = ident `cut'` "identifier"

-- | A parsing error.
data Error
  = Precise Pos ByteString     -- ^ A precisely known error, like leaving out "in" from "let".
  | Imprecise Pos [ByteString] -- ^ An imprecise error, when we expect a number of different things,
                             --   but parse something else.
  deriving (Eq, Show)

errorPos :: Error -> Pos
errorPos (Precise p _)   = p
errorPos (Imprecise p _) = p

-- | Merge two errors. Inner errors (which were thrown at points with more consumed inputs)
--   are preferred. If errors are thrown at identical input positions, we prefer precise errors
--   to imprecise ones.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
merge :: Error -> Error -> Error
merge e e' = case (errorPos e, errorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (p, _)          -> case (e, e') of
    (Precise{}      , _               ) -> e
    (_              , Precise{}       ) -> e'
    (Imprecise _ es , Imprecise _ es' ) -> Imprecise p (es ++ es')
{-# noinline merge #-} -- merge is "cold" code, so we shouldn't inline it.

-- | Pretty print an error. The `ByteString` input is the source file. The offending line from the
--   source is displayed in the output.
prettyError :: ByteString -> Error -> ByteString
prettyError b e =

  let pos :: Pos
      pos      = case e of Imprecise pos _ -> pos
                           Precise pos _   -> pos
      ls       = B.lines b
      (l, c)   = head $ posLineCols b [pos]
      line     = if l < length ls then ls !! l else ""
      linum    = packUTF8 $ show l
      lpad     = B.replicate (B.length linum) ' '

      err (Precise _ e)    = e
      err (Imprecise _ es) = imprec es

      imprec :: [ByteString] -> ByteString
      imprec []     = error "impossible"
      imprec [e]    = e
      imprec (e:es) = e <> go es where
        go []     = ""
        go [e]    = " or " <> e
        go (e:es) = ", " <> e <> go es

  in packUTF8 (show l) <> ":" <> packUTF8 (show c) <> ":\n" <>
     lpad   <> "|\n" <>
     linum  <> "| " <> line <> "\n" <>
     lpad <> "| " <> B.replicate c ' ' <> "^\n" <>
     "parse error: expected " <>
     err e

-- | Imprecise cut: we slap a list of items on inner errors.
cut :: Parser Error a -> [ByteString] -> Parser Error a
cut p es = do
  pos <- getPos
  cutting p (Imprecise pos es) merge

-- | Precise cut: we propagate at most a single error.
cut' :: Parser Error a -> ByteString -> Parser Error a
cut' p e = do
  pos <- getPos
  cutting p (Precise pos e) merge
