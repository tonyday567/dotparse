{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Dot language parsers built on circuits-parser.
--
-- Merged from DotParse.FlatParse and DotParse.FlatParse.TH.
module DotParse.Parser
  ( -- * Error types
    Error (..),
    prettyError,

    -- * Token parsers
    keyword,
    keyword',
    symbol,
    symbol',
    ws,
    token,
    ident,
    ident',
    cut,
    cut',
    char',
    notFollowedBy,

    -- * Running parsers
    testParser,
    runParser_,

    -- * Conversions
    strToUtf8,
    utf8ToStr,

    -- * Character parsers
    int,
    double,
    signed,
    quoted,
    htmlLike,

    -- * Separators and brackets
    sepP,
    wrapSquareP,
    wrapSquarePrint,
    wrapCurlyP,
    wrapCurlyPrint,
    wrapQuotePrint,

    -- * Geometry
    pointP,
    Spline (..),
    splineP,
    rectP,

    -- * Misc
    boolP,
    nonEmptyP,
  )
where

import Circuit (Circuit (Lift))
import Circuit.Parser
import Control.Monad (void)
import Data.Bool
import Data.ByteString hiding (any, empty, filter, head, length, map, null, reverse, zip, zipWith)
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Functor
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text qualified as T
import Data.List.NonEmpty hiding (head, map, reverse)
import GHC.Generics
import NumHask.Space
import Prelude hiding (replicate)

----------------------------------------------------------------------
-- Whitespace & token helpers (from TH)
----------------------------------------------------------------------

isWhitespace :: Char -> Bool
isWhitespace ' '  = True
isWhitespace '\t' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace _    = False

-- | Consume whitespace + comments.
ws :: Parser Text Char ()
ws = skipMany (skipWhile isWhitespace <|> lineComment <|> multilineComment)

-- | Consume whitespace after running a parser.
token :: Parser Text Char a -> Parser Text Char a
token p = p <* ws
{-# INLINE token #-}

-- | Parse a line comment: // ... until newline or EOF
lineComment :: Parser Text Char ()
lineComment = char '/' *> char '/' *> skipMany (satisfy (/= '\n'))

-- | Parse a potentially nested multiline comment.
multilineComment :: Parser Text Char ()
multilineComment = char '/' *> char '*' *> go (1 :: Int)
  where
    go 0 = ws
    go n =
      (string "*/" *> go (n - 1))
        <|> (string "/*" *> go (n + 1))
        <|> (anyToken >> go n)
        <|> pure ()  -- EOF inside comment

-- | Parse a HTML-Like string by counting the angle brackets
htmlLike :: Parser Text Char String
htmlLike = ws *> char '<' *> go (1 :: Int) "<"
  where
    go 0 acc = ws $> reverse acc
    go n acc =
      (char '>' *> go (n - 1) ('>' : acc))
        <|> (char '<' *> go (n + 1) ('<' : acc))
        <|> (anyToken >>= (\c -> go n (c : acc)))

----------------------------------------------------------------------
-- Identifiers (from TH)
----------------------------------------------------------------------

isValidStartChar :: Char -> Bool
isValidStartChar c =
  isAsciiUpper c
    || isAsciiLower c
    || ('\200' <= c && c <= '\377')
    || (c == '_')

isValidChar :: Char -> Bool
isValidChar c = isValidStartChar c || isDigit c

identStartChar :: Parser Text Char Char
identStartChar = satisfy isValidStartChar

identChar :: Parser Text Char Char
identChar = satisfy isValidChar

-- | Parse a non-keyword string.
symbol :: String -> Parser Text Char ()
symbol str = token (void (string str))

-- | Parse a keyword string.
keyword :: String -> Parser Text Char ()
keyword str = token (void (string str))

-- | Parse a keyword string, throw precise error on failure.
keyword' :: String -> Parser Text Char ()
keyword' = keyword

-- | Parse a non-keyword string, precise error variant.
symbol' :: String -> Parser Text Char ()
symbol' = symbol

-- | Parse an identifier.
ident :: Parser Text Char ByteString
ident =
  token $ do
    cs <- (:) <$> identStartChar <*> many identChar
    pure $ B.pack cs

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser Text Char ByteString
ident' = ident

-- | Parse a single character (convenience alias).
char' :: Char -> Parser Text Char ()
char' c = void (char c)

-- | Convenience: match a ByteString literal (byte-wise, matching IsString ByteString encoding)

----------------------------------------------------------------------
-- Error handling (from TH)
----------------------------------------------------------------------

data Error
  = Precise ByteString
  | Imprecise [ByteString]
  deriving (Eq, Show)

prettyError :: ByteString -> Error -> ByteString
prettyError b e =
  let ls = B.lines b
      line = if not (null ls) then head ls else ""
      err (Precise e') = e'
      err (Imprecise es) = imprec es
      imprec [] = "unknown error"
      imprec [e'] = e'
      imprec (e' : es') = e' <> go es'
      go [] = ""
      go [e'] = " or " <> e'
      go (e' : es') = ", " <> e' <> go es'
   in "parse error: expected " <> err e

cut :: Parser Text Char a -> [ByteString] -> Parser Text Char a
cut p _ = p

cut' :: Parser Text Char a -> ByteString -> Parser Text Char a
cut' p _ = p

----------------------------------------------------------------------
-- Conversions (from TH)
----------------------------------------------------------------------

strToUtf8 :: String -> ByteString
strToUtf8 = B.pack . fmap (toEnum . fromEnum)

utf8ToStr :: ByteString -> String
utf8ToStr = fmap (toEnum . fromEnum) . B.unpack

notFollowedBy :: Parser Text Char a -> Parser Text Char ()
notFollowedBy p = Parser $ Lift $ \s ->
  case runParser p s of
    That _    -> These () s
    _         -> That s

----------------------------------------------------------------------
-- Runner helpers (from FlatParse)
----------------------------------------------------------------------

testParser :: (Show a) => Parser Text Char a -> ByteString -> IO ()
testParser p b =
  case runParser p (decodeUtf8With lenientDecode b) of
    That _  -> B.putStrLn "uncaught parse error"
    This a  -> print a
    These a _ -> print a

runParser_ :: Parser Text Char a -> ByteString -> a
runParser_ p b = case runParser p (decodeUtf8With lenientDecode b) of
  These r s | T.null s -> r
  These _ x -> error $ "leftovers: " <> T.unpack x
  This r    -> r
  That _    -> error "parse error"

----------------------------------------------------------------------
-- Numeric parsers (from FlatParse)
----------------------------------------------------------------------

digit :: Parser Text Char Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

int :: Parser Text Char Int
int = token do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser Text Char (Int, Int)
digits = chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))

double :: Parser Text Char Double
double = token do
  (placel, nl) <- digits
  withOption
    (char '.' *> digits)
    ( \(placer, nr) ->
        case (placel, placer) of
          (1, 1) -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

signed :: (Num b) => Parser Text Char b -> Parser Text Char b
signed p = withOption (char '-') (const (((-1) *) <$> p)) p

----------------------------------------------------------------------
-- Quoted strings and separators (from FlatParse)
----------------------------------------------------------------------

quoted :: Parser Text Char String
quoted = symbol "\"" *> many unquoteQuote <* symbol "\""

unquoteQuote :: Parser Text Char Char
unquoteQuote = do
  next <- satisfy (/= '"')
  case next of
    '/' -> (char '"' >> pure '"') <|> pure '/'
    x -> pure x

sepP :: Parser Text Char ()
sepP =
  token
    ( void (string ";")
        <|> void (string ",")
    )

wrapSquareP :: Parser Text Char a -> Parser Text Char a
wrapSquareP p = symbol "[" *> p <* symbol "]"

wrapSquarePrint :: ByteString -> ByteString
wrapSquarePrint b = "[" <> b <> "]"

wrapQuotePrint :: ByteString -> ByteString
wrapQuotePrint b = "\"" <> b <> "\""

wrapCurlyP :: Parser Text Char a -> Parser Text Char a
wrapCurlyP p = symbol "{" *> p <* symbol "}"

wrapCurlyPrint :: ByteString -> ByteString
wrapCurlyPrint b = "{" <> b <> "}"

----------------------------------------------------------------------
-- Geometry parsers (from FlatParse)
----------------------------------------------------------------------

pointP :: Parser Text Char (Point Double)
pointP = token $ Point <$> double <*> (symbol "," *> double)

data Spline = Spline
  { splineEnd :: Maybe (Point Double),
    splineStart :: Maybe (Point Double),
    splineP1 :: Point Double,
    splineTriples :: [(Point Double, Point Double, Point Double)]
  }
  deriving (Eq, Show, Generic)

splineP :: Parser Text Char Spline
splineP =
  Spline
    <$> optional (symbol "e," *> pointP)
    <*> optional (symbol "s" *> pointP)
    <*> pointP
    <*> some ((,,) <$> pointP <*> pointP <*> pointP)

rectP :: Parser Text Char (Rect Double)
rectP = token $ do
  x <- double
  _ <- symbol ","
  y <- double
  _ <- symbol ","
  z <- double
  _ <- symbol ","
  w <- double
  pure $ Rect x z y w

boolP :: Parser Text Char Bool
boolP =
  (True <$ symbol "true")
    <|> (False <$ symbol "false")

nonEmptyP :: Parser Text Char a -> Parser Text Char () -> Parser Text Char (NonEmpty a)
nonEmptyP p sep = token $ do
  s <- p
  xs <- many (optional sep *> p)
  pure (s :| xs)
