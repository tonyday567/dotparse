{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- | Simple parser implementation
module DotParse.Parser
  ( -- * Parser type and running
    Parser,
    Result (..),
    runParser,
    runParser_,
    testParser,

    -- * Error handling
    Error (..),
    prettyError,
    errorPos,
    cut,
    cut',

    -- * Position tracking
    Pos,
    getPos,
    posLineCols,

    -- * Primitives
    satisfy,
    satisfyAscii,
    anyWord8,
    anyChar,
    eof,
    lookahead,
    branch,
    getPos,

    -- * Character classes
    isDigit,
    isAsciiUpper,
    isAsciiLower,

    -- * Combinators
    empty,
    (<|>),
    optional,
    many,
    some,
    skipMany,
    chainr,
    withOption,
    notFollowedBy,

    -- * String matching
    char,
    string,
    byteStringOf,

    -- * Whitespace and tokens
    ws,
    token,
    keyword,
    keyword',
    symbol,
    symbol',
    ident,

    -- * Utilities
    strToUtf8,
    utf8ToStr,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), ap, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.Word (Word8)
import Prelude hiding (replicate)

-- * Types

type Pos = Int

data Result e a
  = OK a ByteString
  | Fail
  | Err e
  deriving (Show, Functor)

newtype Parser e a = Parser
  { runParser :: ByteString -> Result e a
  }
  deriving (Functor)

instance Applicative (Parser e) where
  pure a = Parser $ \s -> OK a s
  (<*>) = ap

instance Monad (Parser e) where
  Parser p >>= f = Parser $ \s -> case p s of
    OK a s' -> runParser (f a) s'
    Fail -> Fail
    Err e -> Err e

instance Alternative (Parser e) where
  empty = Parser $ \_ -> Fail
  Parser p <|> Parser q = Parser $ \s -> case p s of
    Fail -> q s
    ok -> ok

instance MonadPlus (Parser e)

-- * Error handling

data Error
  = Precise Pos ByteString
  | Imprecise Pos [ByteString]
  deriving (Eq, Show)

errorPos :: Error -> Pos
errorPos (Precise p _) = p
errorPos (Imprecise p _) = p

merge :: Error -> Error -> Error
merge e e' = case (errorPos e, errorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (_, _) -> case (e, e') of
    (Precise {}, _) -> e
    (_, Precise {}) -> e'
    (Imprecise p es, Imprecise _ es') -> Imprecise p (es <> es')

prettyError :: ByteString -> Error -> ByteString
prettyError b e =
  let pos = errorPos e
      ls = BC.lines b
      (l, c) = head $ posLineCols b [pos]
      line = if l < length ls then ls !! l else mempty
      linum = strToUtf8 $ show l
      lpad = BC.replicate (BC.length linum) ' '

      err (Precise _ e) = e
      err (Imprecise _ es) = imprec es

      imprec :: [ByteString] -> ByteString
      imprec [] = error "impossible"
      imprec [e] = e
      imprec (e : es) = e <> go es
        where
          go [] = mempty
          go [e] = strToUtf8 " or " <> e
          go (e : es) = strToUtf8 ", " <> e <> go es
   in strToUtf8 (show l)
        <> strToUtf8 ":"
        <> strToUtf8 (show c)
        <> strToUtf8 ":\n"
        <> lpad
        <> strToUtf8 "|\n"
        <> linum
        <> strToUtf8 "| "
        <> line
        <> strToUtf8 "\n"
        <> lpad
        <> strToUtf8 "| "
        <> BC.replicate c ' '
        <> strToUtf8 "^\n"
        <> strToUtf8 "parse error: expected "
        <> err e

-- | Imprecise cut
cut :: Parser Error a -> [ByteString] -> Parser Error a
cut (Parser p) es = Parser $ \s ->
  let pos = B.length s
   in case p s of
        Fail -> Err (Imprecise pos es)
        Err e -> Err (merge (Imprecise pos es) e)
        ok -> ok

-- | Precise cut
cut' :: Parser Error a -> ByteString -> Parser Error a
cut' (Parser p) e = Parser $ \s ->
  let pos = B.length s
   in case p s of
        Fail -> Err (Precise pos e)
        Err e' -> Err (merge (Precise pos e) e')
        ok -> ok

-- * Primitives

satisfy :: (Char -> Bool) -> Parser e Char
satisfy pred = Parser $ \s ->
  if B.null s
    then Fail
    else
      let c = BC.head s
       in if pred c
            then OK c (B.tail s)
            else Fail

satisfyAscii :: (Char -> Bool) -> Parser e Char
satisfyAscii = satisfy

anyWord8 :: Parser e Word8
anyWord8 = Parser $ \s ->
  if B.null s
    then Fail
    else OK (B.head s) (B.tail s)

anyChar :: Parser e Char
anyChar = Parser $ \s ->
  if B.null s
    then Fail
    else OK (BC.head s) (B.tail s)

eof :: Parser e ()
eof = Parser $ \s ->
  if B.null s
    then OK () s
    else Fail

getPos :: Parser e Pos
getPos = Parser $ \s -> OK (B.length s) s

-- * Combinators

optional :: Parser e a -> Parser e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

skipMany :: Parser e a -> Parser e ()
skipMany p = go
  where
    go = (p *> go) <|> pure ()

chainr :: (a -> b -> b) -> Parser e a -> Parser e b -> Parser e b
chainr f p z = go
  where
    go = (f <$> p <*> go) <|> z

withOption :: Parser e a -> (a -> Parser e b) -> Parser e b -> Parser e b
withOption p f def = (p >>= f) <|> def

lookahead :: Parser e a -> Parser e a
lookahead (Parser p) = Parser $ \s -> case p s of
  OK a _ -> OK a s
  Fail -> Fail
  Err e -> Err e

branch :: Parser e a -> Parser e b -> Parser e b -> Parser e b
branch test yes no = (test *> yes) <|> no

notFollowedBy :: Parser e a -> Parser e b -> Parser e a
notFollowedBy p q = do
  a <- p
  Parser $ \s -> case runParser q s of
    Fail -> OK a s
    _ -> Fail

-- * String matching

char :: Char -> Parser e ()
char c = void $ satisfy (== c)

string :: String -> Parser e ()
string str = Parser $ \s ->
  let bs = strToUtf8 str
   in if bs `B.isPrefixOf` s
        then OK () (B.drop (B.length bs) s)
        else Fail

byteStringOf :: Parser e a -> Parser e ByteString
byteStringOf (Parser p) = Parser $ \s ->
  case p s of
    OK _ s' ->
      let consumed = B.take (B.length s - B.length s') s
       in OK consumed s'
    Fail -> Fail
    Err e -> Err e

-- * Whitespace and identifiers

isValidStartChar :: Char -> Bool
isValidStartChar c =
  isAsciiUpper c
    || isAsciiLower c
    || ('\200' <= c && c <= '\377')
    || (c == '_')

isValidChar :: Char -> Bool
isValidChar c = isValidStartChar c || isDigit c

identStartChar :: Parser e Char
identStartChar = satisfy isValidStartChar

identChar :: Parser e Char
identChar = satisfy isValidChar

ws :: Parser e ()
ws = go
  where
    go =
      ( char ' ' *> ws
      )
        <|> ( char '\\' *> char '\n' *> ws
            )
        <|> ( char '\\' *> char '\\' *> char '\\' *> char '\n' *> ws
            )
        <|> ( char '\n' *> ws
            )
        <|> ( char '\t' *> ws
            )
        <|> ( char '\r' *> ws
            )
        <|> ( char ';' *> ws
            )
        <|> ( string "//" *> lineComment
            )
        <|> ( string "/*" *> multilineComment
            )
        <|> pure ()

lineComment :: Parser e ()
lineComment =
  withOption
    anyWord8
    ( \case
        10 -> ws
        _ -> lineComment
    )
    (pure ())

multilineComment :: Parser e ()
multilineComment = go (1 :: Int)
  where
    go 0 = ws
    go n =
      ( string "*/" *> go (n - 1)
      )
        <|> ( string "/*" *> go (n + 1)
            )
        <|> branch anyWord8 (go n) (pure ())

token :: Parser e a -> Parser e a
token p = p <* ws

symbol :: String -> Parser e ()
symbol str = token (string str)

keyword :: String -> Parser e ()
keyword str = token (string str `notFollowedBy` identChar)

symbol' :: String -> Parser Error ()
symbol' str = symbol str `cut'` strToUtf8 str

keyword' :: String -> Parser Error ()
keyword' str = keyword str `cut'` strToUtf8 str

ident :: Parser e ByteString
ident = token $ byteStringOf $ identStartChar *> skipMany identChar

-- * Utilities

strToUtf8 :: String -> ByteString
strToUtf8 = BC.pack

utf8ToStr :: ByteString -> String
utf8ToStr = BC.unpack

posLineCols :: ByteString -> [Pos] -> [(Int, Int)]
posLineCols input positions =
  let inputStr = utf8ToStr input
      lines' = lines inputStr
      lineLengths = map ((+ 1) . length) lines' -- +1 for newline
      cumulative = scanl (+) 0 lineLengths

      findLineCol pos =
        let (lineNum, lineStart) = go 0 cumulative
            colNum = pos - lineStart
         in (lineNum, colNum)
        where
          go lineNum (start : next : rest)
            | pos < next = (lineNum, start)
            | otherwise = go (lineNum + 1) (next : rest)
          go lineNum [start] = (lineNum, start)
          go _ [] = (0, 0)
   in map findLineCol positions

-- * Test runner

testParser :: (Show a) => Parser Error a -> ByteString -> IO ()
testParser p b =
  case runParser p b of
    Err e -> BC.putStrLn $ prettyError b e
    OK a _ -> print a
    Fail -> BC.putStrLn $ strToUtf8 "uncaught parse error"

runParser_ :: Parser Error a -> ByteString -> a
runParser_ p b = case runParser p b of
  OK r rest | B.null rest -> r
  OK _ x -> error $ utf8ToStr $ strToUtf8 "leftovers: " <> x
  Fail -> error "Fail"
  Err e -> error $ utf8ToStr $ prettyError b e
