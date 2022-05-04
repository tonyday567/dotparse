{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use unwords" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use ?~" #-}

-- | Abstract Grammar from:
-- http://www.graphviz.org/doc/info/lang.html

module Dot where

import FlatParse.Basic hiding (cut, lines)
import Dot.TH hiding (merge)
import Data.Char hiding (isDigit)
import qualified Data.ByteString.Char8 as C
import GHC.Generics
import NeatInterpolation
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString hiding (zip, zipWith, putStrLn, map, length, head, empty)
import Data.Proxy
import Data.List.NonEmpty hiding (zip, zipWith, map, (!!), length, head)
import Data.Bool
import Data.These
import Prelude hiding (replicate)
import qualified Data.ByteString.Char8 as B
import Control.Monad
import System.Process.ByteString
import System.Exit
import qualified Algebra.Graph as G
import NumHask.Space
import qualified Data.Map.Strict as Map
import Optics.Core
import GHC.IO.Unsafe
import Data.Text (Text)
import Chart
import Data.Maybe
import Data.Map.Merge.Strict
import qualified Data.Text as Text
import qualified Algebra.Graph.Labelled as L
import Data.Monoid
import Data.Bifunctor
-- import Data.Algorithm.DiffOutput
-- import Data.Algorithm.Diff

-- $setup
-- >>> import Chart.Dot
-- >>> import FlatParse.Basic
-- >>> import NeatInterpolation
-- >>> import Data.Text.Encoding (encodeUtf8)
-- >>> import Data.Proxy
-- >>> :set -XOverloadedStrings

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser Error a -> ByteString -> IO ()
testParser p b =
  case runParser p b of
    Err e  -> B.putStrLn $ prettyError b e
    OK a _ -> print a
    Fail   -> B.putStrLn "uncaught parse error"

-- | dotParse and then dotPrint:
--
-- - pretty printing error on failure.
--
-- - This is not an exact parser/printer, so the test re-parses the dotPrint, which should be idempotent
testParserP :: forall a. (DotParse a) => Proxy a -> ByteString -> IO ()
testParserP _ b =
  case runParser dotParse b :: Result Error a of
    Err e -> B.putStrLn $ prettyError b e
    OK a left -> do
      when (left /= "") (B.putStrLn $ "parsed with leftovers: " <> left)
      case runParser dotParse (dotPrint a) :: Result Error a of
        Err e -> B.putStrLn $ "round trip error: " <> prettyError (dotPrint a) e
        Fail -> B.putStrLn "uncaught round trip parse error"
        OK _ left -> do
          when (left /= "") (B.putStrLn $ "round trip parse with left overs" <> left)
    Fail -> B.putStrLn "uncaught parse error"

-- | run a dotParse erroring on leftovers, Fail or Err
runDotParser :: (DotParse a) => ByteString -> a
runDotParser b = case runParser dotParse b of
  OK r "" -> r
  OK _ x -> error $ unpackUTF8 $ "leftovers: " <> x
  Fail -> error "Fail"
  Err e -> error $ unpackUTF8 $ prettyError b e

-- | run a Parser, erroring on leftovers, Fail or Err
runParser_ :: Parser Error a -> ByteString -> a
runParser_ p b = case runParser p b of
  OK r "" -> r
  OK _ x -> error $ unpackUTF8 $ "leftovers: " <> x
  Fail -> error "Fail"
  Err e -> error $ unpackUTF8 $ prettyError b e

-- * printing
class DotParse a where
  dotPrint :: a -> ByteString
  dotParse :: Parser Error a

-- * Dot Grammar


-- | MergeEdges (strict)
--
-- >>> dtrip MergeEdges
-- True
--
-- >>> dtrip NoMergeEdges
-- True
data MergeEdges = MergeEdges | NoMergeEdges deriving (Eq, Show, Generic)

instance DotParse MergeEdges
  where
    dotPrint MergeEdges = "strict"
    dotPrint NoMergeEdges = ""

    dotParse = token $ optioned $(keyword "strict") (const $ pure MergeEdges) (pure NoMergeEdges)

dtrip :: (Eq a, DotParse a) => a -> Bool
dtrip a = [(a,"")] == [(r,rem)|(OK r rem) <- [runParser dotParse (dotPrint a)]]

btrip :: forall a. (DotParse a) => Proxy a -> ByteString -> Bool
btrip _ b = [b] == (dotPrint <$> [r | (OK r "") <- [runParser dotParse b :: Result Error a]])

-- | Directed (digraph | graph)
-- >>> dtrip Directed
-- True
--
-- >>> dtrip UnDirected
-- True
--
-- >>> btrip (Proxy :: Proxy Directed) "digraph"
-- True
--
-- >>> btrip (Proxy :: Proxy Directed) "graph"
-- True
data Directed = Directed | UnDirected deriving (Eq, Show, Generic)

instance DotParse Directed
  where
    dotPrint Directed = "digraph"
    dotPrint UnDirected = "graph"

    dotParse = token $
      (Directed <$ $(keyword "digraph")) <|>
      (UnDirected <$ $(keyword "graph"))

-- |
--
-- >>> runDotParser "0" :: ID
-- IDInt 0
--
-- >>> runDotParser "-.123" :: ID
-- IDDouble (-0.123)
--
-- >>> runParser dotParse "apple_1'" :: Result () ID
-- OK (IDString "apple_1") "'"
--
-- FIXME:
-- >>> :set -XQuasiQuotes
-- >>> runParser dotParse $ encodeUtf8 [trimming|"quoted \""|] :: Result () ID
-- OK (IDQuoted "quoted \\") "\""
--
-- >>> runDotParser (encodeUtf8 [trimming|<The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>|]) :: ID
-- IDHtml "<The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>"
data ID = IDString String | IDInt Int | IDDouble Double | IDQuoted String | IDHtml String deriving (Eq, Show, Generic, Ord)

instance DotParse ID
  where
    dotPrint (IDString s) = packUTF8 s
    dotPrint (IDInt i) = packUTF8 (show i)
    dotPrint (IDDouble x) = packUTF8 (show x)
    dotPrint (IDQuoted x) =
      packUTF8 (show x)
    dotPrint (IDHtml s) = packUTF8 s

    -- order matters
    dotParse =
      (IDString . C.unpack <$> ident) <|>
      (IDInt <$> (signed int `notFollowedBy` $(char '.'))) <|>
      (IDDouble <$> signed double) <|>
      (IDQuoted <$> quoted) <|>
      (IDHtml <$> htmlLike)

label :: ID -> String
label (IDString s) = s
label (IDInt i) = show i
label (IDDouble d) = show d
label (IDQuoted q) = q
label (IDHtml h) = h

-- | Attribute tuple
--
-- >>> runDotParser "shape=diamond" :: IDStatement
-- IDStatement {firstID = IDString "shape", secondID = IDString "diamond"}
data IDStatement = IDStatement { firstID :: ID, secondID :: ID } deriving (Eq, Show, Generic)

instance DotParse IDStatement
  where
    dotPrint (IDStatement x0 x1) = dotPrint x0 <> "=" <> dotPrint x1

    dotParse = token $
      do
        x0 <- token dotParse
        _ <- token $(symbol "=")
        x1 <- dotParse
        pure $ IDStatement x0 x1


data AttributeType = GraphType | NodeType | EdgeType deriving (Eq, Show, Generic)

instance DotParse AttributeType
  where
    dotPrint GraphType = "graph"
    dotPrint NodeType = "node"
    dotPrint EdgeType = "edge"

    dotParse = token
      $(switch [| case _ of
                   "graph"  -> pure GraphType
                   "node" -> pure NodeType
                   "edge" -> pure EdgeType
                |])

data Compass = CompassN | CompassNE | CompassE | CompassSE | CompassS | CompassSW | CompassW | CompassNW | CompassC | Compass_ deriving (Eq, Show, Generic)

instance DotParse Compass
  where
    dotPrint CompassN = "n"
    dotPrint CompassNE = "ne"
    dotPrint CompassE = "e"
    dotPrint CompassSE = "se"
    dotPrint CompassS = "s"
    dotPrint CompassSW = "sw"
    dotPrint CompassW = "w"
    dotPrint CompassNW = "nw"
    dotPrint CompassC = "c"
    dotPrint Compass_ = "_"

    dotParse = token
      $(switch [| case _ of
                   "n"  -> pure CompassN
                   "ne"  -> pure CompassNE
                   "e"  -> pure CompassE
                   "se"  -> pure CompassSE
                   "s"  -> pure CompassS
                   "sw"  -> pure CompassSW
                   "w"  -> pure CompassW
                   "nw"  -> pure CompassNW
                   "c"  -> pure CompassC
                   "_" -> pure Compass_
                |])


newtype Port = Port { portID :: These ID Compass } deriving (Eq, Show, Generic)

instance DotParse Port
  where
    dotPrint (Port (This i)) = ": " <> dotPrint i
    dotPrint (Port (That c)) = ": " <> dotPrint c
    dotPrint (Port (These i c)) = ": " <> dotPrint i <> " : " <> dotPrint c

    dotParse = token $
      ((\x0 x1 -> Port (These x0 x1)) <$> ($(symbol ":") *> dotParse) <*> ($(symbol ":") *> dotParse)) <|>
      (Port . This <$> ($(symbol ":") *> dotParse)) <|>
      (Port . That <$> ($(symbol ":") *> dotParse))

-- |
data NodeID = NodeID { nodeID' :: ID, nodePort :: Maybe Port } deriving (Eq, Show, Generic)

instance DotParse NodeID
  where
    dotPrint (NodeID x0 p) = intercalate " " $ [dotPrint x0] <> maybe [] ((:[]) . dotPrint) p

    dotParse = token $ NodeID <$> dotParse <*> optional dotParse

-- |
-- >>> runDotParser "shape=diamond; color=blue" :: AList
-- AList {aList = IDStatement {firstID = IDString "shape", secondID = IDString "diamond"} :| [IDStatement {firstID = IDString "color", secondID = IDString "blue"}]}
newtype AList = AList { aList :: NonEmpty IDStatement } deriving (Eq, Show, Generic)

instance DotParse AList
  where
    dotPrint (AList xs) = intercalate ";" $ dotPrint <$> toList xs

    dotParse = token $ do
      s <- dotParse
      xs <- many (optional sepP *> dotParse)
      pure $ AList (s :| xs)

-- |
-- >>> runDotParser "[shape=diamond; color=blue]" :: AttrList
-- AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "shape", secondID = IDString "diamond"} :| [IDStatement {firstID = IDString "color", secondID = IDString "blue"}]}]}
newtype AttrList = AttrList { attrList :: [AList] } deriving (Eq, Show, Generic)

instance DotParse AttrList
  where
    dotPrint (AttrList xs) = case xs of
      [] -> ""
      xs' -> intercalate " " $ wrapSquarePrint . dotPrint <$> xs'

    dotParse = token $
      (AttrList <$> many (wrapSquareP dotParse)) <|>
      (AttrList [] <$ wrapSquareP ws)

data AttributeStatement = AttributeStatement { attributeType :: AttributeType, attributeList :: AttrList  } deriving (Eq, Show, Generic)

instance DotParse AttributeStatement
  where
    dotPrint (AttributeStatement t l) = intercalate " " [dotPrint t, dotPrint l]

    dotParse = AttributeStatement <$> dotParse <*> dotParse

-- |
-- >>> runDotParser "A [shape=diamond; color=blue]" :: Statement
-- StatementNode (NodeStatement {nodeID = NodeID {nodeID' = IDString "A", nodePort = Nothing}, nodeAttributes = AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "shape", secondID = IDString "diamond"} :| [IDStatement {firstID = IDString "color", secondID = IDString "blue"}]}]}})
data NodeStatement = NodeStatement { nodeID :: NodeID, nodeAttributes :: AttrList } deriving (Eq, Show, Generic)

instance DotParse NodeStatement
  where
    dotPrint (NodeStatement i l) = intercalate " " [dotPrint i, dotPrint l]

    dotParse = NodeStatement <$> dotParse <*> dotParse

-- |
newtype EdgeID = EdgeID { unedgeID :: Either NodeID SubGraphStatement} deriving (Eq, Show, Generic)

instance DotParse EdgeID
  where
    dotPrint (EdgeID e) = either dotPrint dotPrint e

    dotParse = EdgeID <$> (Left <$> dotParse) <|> (Right <$> dotParse)

-- An edgeop is -> in directed graphs and -- in undirected graphs.
data EdgeOp = EdgeDirected | EdgeUndirected deriving (Eq, Show, Generic)

instance DotParse EdgeOp
  where
    dotPrint EdgeDirected = "->"
    dotPrint EdgeUndirected = "--"

    dotParse = token
      $(switch [| case _ of
                   "->"  -> pure EdgeDirected
                   "--" -> pure EdgeUndirected
                |])

-- |
-- >>> runDotParser "-> B" :: EdgeRHS
-- EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "B", nodePort = Nothing})}}
data EdgeRHS = EdgeRHS { edgeOp :: EdgeOp, edgeID :: EdgeID } deriving (Eq, Show, Generic)

instance DotParse EdgeRHS
  where
    dotPrint (EdgeRHS o e) = intercalate " " [dotPrint o, dotPrint e]

    dotParse = token $
      EdgeRHS <$> dotParse <*> dotParse

-- |
-- >>> runDotParser "-> B -> C" :: EdgeRHSs
-- EdgeRHSs {edgeRHSs = EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "B", nodePort = Nothing})}} :| [EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "C", nodePort = Nothing})}}]}
newtype EdgeRHSs = EdgeRHSs { edgeRHSs :: NonEmpty EdgeRHS } deriving (Eq, Show, Generic)

instance DotParse EdgeRHSs
  where
    dotPrint (EdgeRHSs xs) = intercalate " " (dotPrint <$> toList xs)

    dotParse = token $
      (\x0 x1 -> EdgeRHSs (x0:|x1)) <$> dotParse <*> many dotParse

-- |
-- >>> runDotParser "A -> B [style=dashed, color=grey]" :: EdgeStatement
-- EdgeStatement {edgeStatementID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "A", nodePort = Nothing})}, edgeStatementRHS = EdgeRHSs {edgeRHSs = EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "B", nodePort = Nothing})}} :| []}, edgeStatementAttributes = AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "style", secondID = IDString "dashed"} :| [IDStatement {firstID = IDString "color", secondID = IDString "grey"}]}]}}
data EdgeStatement = EdgeStatement { edgeStatementID :: EdgeID, edgeStatementRHS :: EdgeRHSs, edgeStatementAttributes :: AttrList } deriving (Eq, Show, Generic)

instance DotParse EdgeStatement
  where
    dotPrint (EdgeStatement x r xs) = intercalate " " [dotPrint x, dotPrint r, dotPrint xs]

    dotParse = token $
      EdgeStatement <$> dotParse <*> dotParse <*> dotParse

data Statement = StatementNode NodeStatement | StatementEdge EdgeStatement | StatementAttribute AttributeStatement | StatementID IDStatement | StatementSubGraph SubGraphStatement deriving (Eq, Show, Generic)

instance DotParse Statement
  where
    dotPrint (StatementNode x) = dotPrint x
    dotPrint (StatementEdge x) = dotPrint x
    dotPrint (StatementAttribute x) = dotPrint x
    dotPrint (StatementID x) = dotPrint x
    dotPrint (StatementSubGraph x) = dotPrint x

    dotParse = token (
      -- Order is important
      (StatementEdge <$> dotParse) <|>
      (StatementID <$> dotParse) <|>
      (StatementAttribute <$> dotParse) <|>
      (StatementNode <$> dotParse) <|>
      (StatementSubGraph <$> dotParse))

-- each subgraph must have a unique name
data SubGraphStatement = SubGraphStatement { subgraphID :: Maybe ID, subgraphStatements :: [Statement] } deriving (Eq, Show, Generic)

instance DotParse SubGraphStatement
  where
    dotPrint (SubGraphStatement x xs) =
      intercalate " " $
      maybe []
      (\x' -> [intercalate " " ["subgraph", dotPrint x']]) x <>
      (:[]) (wrapCurlyPrint (intercalate "\n    " $ dotPrint <$> xs))

    dotParse = token $ do
      x <- optional ($(keyword "subgraph") *> dotParse)
      pure (SubGraphStatement x) <*> wrapCurlyP (many (optional sepP *> dotParse))

data Graph = Graph { mergeEdges :: MergeEdges, directed :: Directed, graphid :: Maybe ID, statements :: [Statement]  } deriving (Eq, Show, Generic)


outercalate :: ByteString -> [ByteString] -> ByteString
outercalate _ [] = mempty
outercalate a xs = a <> intercalate a xs <> a

instance DotParse Graph
  where
    dotPrint (Graph me d x xs) =
      intercalate " " $ bool [] ["strict"] (me == MergeEdges) <> [dotPrint d] <> maybe [] ((:[]) . dotPrint) x <> [wrapCurlyPrint (outercalate "\n    " (dotPrint <$> xs))]

    dotParse = token $
      Graph <$>
      dotParse <*>
      dotParse <*>
      optional dotParse <*>
      wrapCurlyP (many dotParse)

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

quoted :: Parser Error String
quoted =
  $(symbol "\"") *> many unquoteQuote <* $(symbol' "\"")

unquoteQuote :: Parser Error Char
unquoteQuote = do
  next <- satisfy (/= '"')
  case next of
    '/' -> branch (lookahead $(char '"')) ('"' <$ $(char '"')) (pure '/')
    x -> pure x

sepP :: Parser e ()
sepP = token
  $(switch [| case _ of
    ";"  -> pure ()
    "," -> pure ()
            |])

wrapSquareP :: Parser Error a -> Parser Error a
wrapSquareP p =
  $(symbol "[") *> p <* $(symbol' "]")

wrapSquarePrint :: ByteString -> ByteString
wrapSquarePrint b = "[" <> b <> "]"

wrapQuotePrint :: ByteString -> ByteString
wrapQuotePrint b = "\"" <> b <> "\""


wrapCurlyP :: Parser Error a -> Parser Error a
wrapCurlyP p = $(symbol "{") *> p <* $(symbol' "}")

wrapCurlyPrint :: ByteString -> ByteString
wrapCurlyPrint b = "{" <> b <> "}"

pointP :: Parser Error (Point Double)
pointP = token $ Point <$> double <*> ($(symbol ",") *> double)

curveP :: Parser Error [Point Double]
curveP = $(symbol "e,") *> many pointP

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

boolP :: Parser Error Bool
boolP =
  (True <$ $(symbol "true")) <|>
  (False <$ $(symbol "false"))


-- * examples

-- | minimal definition
-- >>> runDotParser ex0 :: Graph
-- Graph {mergeEdges = NoMergeEdges, directed = UnDirected, graphid = Nothing, statements = []}
--
-- >>> testParserP (Proxy :: Proxy Graph) ex0
--
ex0 :: ByteString
ex0 = encodeUtf8 [trimming|
graph {}
|]

-- | Examples from https://renenyffenegger.ch/notes/tools/Graphviz/examples/index
--
-- >>> testParserP (Proxy :: Proxy Graph) ex1
--
ex1 :: ByteString
ex1 = encodeUtf8 [trimming|
digraph D {
    A [shape=diamond]
    B [shape=box]
    C [shape=circle]
    A -> B [style=dashed, color=grey]
    A -> C [color="black:invis:black"]
    A -> D [penwidth=5, arrowhead=none]
    }
|]

-- |
--
ex1' :: Graph
ex1' = Graph {mergeEdges = NoMergeEdges, directed = Directed, graphid = Just (IDString "D"), statements = [StatementNode (NodeStatement {nodeID = NodeID {nodeID' = IDString "A", nodePort = Nothing}, nodeAttributes = AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "shape", secondID = IDString "diamond"} :| []}]}}),StatementNode (NodeStatement {nodeID = NodeID {nodeID' = IDString "B", nodePort = Nothing}, nodeAttributes = AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "shape", secondID = IDString "box"} :| []}]}}),StatementNode (NodeStatement {nodeID = NodeID {nodeID' = IDString "C", nodePort = Nothing}, nodeAttributes = AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "shape", secondID = IDString "circle"} :| []}]}}),StatementEdge (EdgeStatement {edgeStatementID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "A", nodePort = Nothing})}, edgeStatementRHS = EdgeRHSs {edgeRHSs = EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "B", nodePort = Nothing})}} :| []}, edgeStatementAttributes = AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "style", secondID = IDString "dashed"} :| [IDStatement {firstID = IDString "color", secondID = IDString "grey"}]}]}}),StatementEdge (EdgeStatement {edgeStatementID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "A", nodePort = Nothing})}, edgeStatementRHS = EdgeRHSs {edgeRHSs = EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "C", nodePort = Nothing})}} :| []}, edgeStatementAttributes = AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "color", secondID = IDQuoted "black:invis:black"} :| []}]}}),StatementEdge (EdgeStatement {edgeStatementID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "A", nodePort = Nothing})}, edgeStatementRHS = EdgeRHSs {edgeRHSs = EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "D", nodePort = Nothing})}} :| []}, edgeStatementAttributes = AttrList {attrList = [AList {aList = IDStatement {firstID = IDString "penwidth", secondID = IDInt 5} :| [IDStatement {firstID = IDString "arrowhead", secondID = IDString "none"}]}]}})]}

-- |
-- >>> testParserP (Proxy :: Proxy Graph) ex2
ex2 :: ByteString
ex2 = encodeUtf8 [trimming|
digraph D {

    node [fontname="Arial"];

    node_A [shape=record    label="shape=record|{above|middle|below}|right"];
    node_B [shape=plaintext label="shape=plaintext|{curly|braces and|bars without}|effect"];

}
|]

-- FIXME:
-- |
-- >>> testParserP (Proxy :: Proxy Graph) ex3
ex3 :: ByteString
ex3 = encodeUtf8 [trimming|
digraph D {
  A -> {B, C, D} -> {F}
}
|]

ex4 :: ByteString
ex4 = encodeUtf8 [trimming|
digraph L {

  node [shape=record fontname=Arial];

  a  [label="one\ltwo three\lfour five six seven\l"]
  b  [label="one\ntwo three\nfour five six seven"]
  c  [label="one\rtwo three\rfour five six seven\r"]

  a -> b -> c

}
|]

ex5 :: ByteString
ex5 = encodeUtf8 [trimming|
digraph D {

  label = "The foo, the bar and the baz";
  labelloc = "t"; // place the label at the top (b seems to be default)

  node [shape=plaintext]

  FOO -> {BAR, BAZ};


}
|]

ex6 :: ByteString
ex6 = encodeUtf8 [trimming|
digraph D {

  label = <The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>;
  labelloc = "t"; // place the label at the top (b seems to be default)

  node [shape=plaintext]

  FOO -> {BAR, BAZ};

}
|]


ex7 :: ByteString
ex7 = encodeUtf8 [trimming|
digraph R {

  node [shape=record];

  { rank=same rA sA tA }
  { rank=same uB vB wB }


   rA -> sA;
   sA -> vB;
   t  -> rA;
   uB -> vB;
   wB -> u;
   wB -> tA;

}
|]

ex8 :: ByteString
ex8 = encodeUtf8 [trimming|
digraph Q {

  node [shape=record];


  nd_1   [label = "Node 1"];
  nd_2   [label = "Node 2"];
  nd_3_a [label = "Above Right Node 3"];
  nd_3_l [label = "Left of Node 3"];
  nd_3   [label = "Node 3"];
  nd_3_r [label = "Right of Node 3"];
  nd_4   [label = "Node 4"];


  nd_3_a -> nd_3_r;
  nd_1 -> nd_2 -> nd_3 -> nd_4;

  subgraph cluster_R {

    {rank=same nd_3_l nd_3 nd_3_r}

    nd_3_l -> nd_3 -> nd_3_r [color=grey arrowhead=none];

  }

}
|]

ex9 :: ByteString
ex9 = encodeUtf8 [trimming|
digraph D {

  subgraph cluster_p {
    label = "Parent";

    subgraph cluster_c1 {
      label = "Child one";
      a;

      subgraph cluster_gc_1 {
        label = "Grand-Child one";
         b;
      }
      subgraph cluster_gc_2 {
        label = "Grand-Child two";
          c;
          d;
      }

    }

    subgraph cluster_c2 {
      label = "Child two";
      e;
    }
  }
}
|]

ex10 :: ByteString
ex10 = encodeUtf8 [trimming|
digraph H {

  aHtmlTable [
   shape=plaintext
   color=blue      // The color of the border of the table
   label=<

     <table border='1' cellborder='0'>
       <tr><td>col 1</td><td>foo</td></tr>
       <tr><td>COL 2</td><td>bar</td></tr>
     </table>

  >];

}|]

ex11 :: ByteString
ex11 = encodeUtf8 [trimming|
digraph {

  tbl [

    shape=plaintext
    label=<

      <table border='0' cellborder='1' color='blue' cellspacing='0'>
        <tr><td>foo</td><td>bar</td><td>baz</td></tr>
        <tr><td cellpadding='4'>
          <table color='orange' cellspacing='0'>
            <tr><td>one  </td><td>two  </td><td>three</td></tr>
            <tr><td>four </td><td>five </td><td>six  </td></tr>
            <tr><td>seven</td><td>eight</td><td>nine </td></tr>
          </table>
        </td>
        <td colspan='2' rowspan='2'>
          <table color='pink' border='0' cellborder='1' cellpadding='10' cellspacing='0'>
            <tr><td>eins</td><td>zwei</td><td rowspan='2'>drei<br/>sechs</td></tr>
            <tr><td>vier</td><td>fünf</td>                             </tr>
          </table>
        </td>
        </tr>

        <tr><td>abc</td></tr>

      </table>

    >];

}
|]

ex12 :: ByteString
ex12 = encodeUtf8 [trimming|
digraph D {

  node [shape=plaintext]

  some_node [
   label=<
     <table border="0" cellborder="1" cellspacing="0">
       <tr><td bgcolor="yellow">Foo</td></tr>
       <tr><td bgcolor="lightblue"><font color="#0000ff">Bar</font></td></tr>
       <tr><td bgcolor="#f0e3ff"><font color="#ff1020">Baz</font></td></tr>
     </table>>
  ];
}
|]

ex13 :: ByteString
ex13 = encodeUtf8 [trimming|
digraph H {

  aHtmlTable [
   shape=plaintext
   label=<

     <table border='1' cellborder='0' style='rounded'>
       <tr><td>col 1</td><td>foo</td></tr>
       <tr><td>COL 2</td><td>bar</td></tr>
     </table>

  >];

}
|]

ex14 :: ByteString
ex14 = encodeUtf8 [trimming|
digraph H {

  parent [
   shape=plaintext
   label=<
     <table border='1' cellborder='1'>
       <tr><td colspan="3">The foo, the bar and the baz</td></tr>
       <tr><td port='port_one'>First port</td><td port='port_two'>Second port</td><td port='port_three'>Third port</td></tr>
     </table>
  >];

  child_one [
   shape=plaintext
   label=<
     <table border='1' cellborder='0'>
       <tr><td>1</td></tr>
     </table>
  >];

  child_two [
   shape=plaintext
   label=<
     <table border='1' cellborder='0'>
       <tr><td>2</td></tr>
     </table>
  >];

  child_three [
   shape=plaintext
   label=<
     <table border='1' cellborder='0'>
       <tr><td>3</td></tr>
     </table>
  >];

  parent:port_one   -> child_one;
  parent:port_two   -> child_two;
  parent:port_three -> child_three;

}
|]

ex15 :: ByteString
ex15 = encodeUtf8 [trimming|
digraph D {

  node [shape=plaintext fontname="Sans serif" fontsize="8"];

  task_menu [ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 1</b></td></tr>
     <tr><td align="left">Choose Menu</td></tr>
     <tr><td align="left"><font color="darkgreen">done</font></td></tr>
   </table>>];

  task_ingredients [ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 2</b></td></tr>
     <tr><td align="left">Buy ingredients</td></tr>
     <tr><td align="left"><font color="darkgreen">done</font></td></tr>
   </table>>];

  task_invitation [ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 4</b></td></tr>
     <tr><td align="left">Send invitation</td></tr>
     <tr><td align="left"><font color="darkgreen">done</font></td></tr>
   </table>>];

  task_cook [ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 5</b></td></tr>
     <tr><td align="left">Cook</td></tr>
     <tr><td align="left"><font color="red">todo</font></td></tr>
   </table>>];

  task_table[ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 3</b></td></tr>
     <tr><td align="left">Lay table</td></tr>
     <tr><td align="left"><font color="red">todo</font></td></tr>
   </table>>];

  task_eat[ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 6</b></td></tr>
     <tr><td align="left">Eat</td></tr>
     <tr><td align="left"><font color="red">todo</font></td></tr>
   </table>>];


  task_menu        -> task_ingredients;
  task_ingredients -> task_cook;
  task_invitation  -> task_cook;
  task_table       -> task_eat;
  task_cook        -> task_eat;

}
|]

testAll :: IO ()
testAll = zipWithM_ (>>)
  (putStrLn <$> ["ex0","ex1","ex2","ex3","ex4","ex5","ex6","ex7"
                ,"ex8","ex9","ex10","ex11","ex12","ex13","ex14","ex15"])
  (testParserP (Proxy :: Proxy Graph) <$>
  [ex0,ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,ex13,ex14,ex15])


defaultBS :: ByteString
defaultBS = encodeUtf8 [trimming|
digraph {
    node [shape=circle
         ,height=0.5];
    graph [overlap=false
          ,splines=spline
          ,size="1!"];
    edge [arrowsize=0];
  }
|]

defaultGraph :: Graph
defaultGraph = runDotParser defaultBS

-- | -Tdot for DotOutput
dotB :: Directed -> ByteString -> IO ByteString
dotB d i = do
  let cmd = case d of
        Directed -> "dot"
        UnDirected -> "neato"
  let args' = ["-Tdot"]
  (r,i,e) <- readProcessWithExitCode cmd args' i
  bool
    (error $ unpackUTF8 e)
    (pure i)
    (r==ExitSuccess)

example1' :: G.Graph Int
example1' = G.edges $
    [(v, (v + 1) `mod` 6) | v <- [0 .. 5]]
        ++ [(v, v + k) | v <- [0 .. 5], k <- [6, 12]]
        ++ [(2, 18), (2, 19), (15, 18), (15, 19), (18, 3), (19, 3)]

toStatementsInt :: G.Graph Int -> [Statement]
toStatementsInt g =
  ((\x -> StatementNode $ NodeStatement (NodeID (IDInt x) Nothing) (AttrList [])) <$> G.vertexList g) <>
  ((\(x, y) ->
      StatementEdge $
      EdgeStatement
      (EdgeID (Left (NodeID (IDInt x) Nothing)))
      (EdgeRHSs $ fromList [EdgeRHS EdgeDirected (EdgeID (Left (NodeID (IDInt y) Nothing)))])
      (AttrList [])) <$> G.edgeList g)

-- |
-- >>> g1 = defaultGrap & #statements %~ (<> toStatementsInt example1')
-- >>> B.putStrLn =<< dotPrint <$> processGraph g1
-- digraph {
--     graph [bb="0,0,495.65,493.78";overlap=true;size="1!";splines=spline]
--     node [height=0.5;label="\\N";shape=circle]
--     edge [arrowsize=0]
--     0 [pos="384.5,475.78";width=0.5]
--     1 [pos="357.5,401.63";width=0.5]
--     0 -> 1 [pos="e,363.57,418.85 378.51,458.77 374.1,446.99 368.12,431.02 363.67,419.13"]
--     6 [pos="411.5,401.63";width=0.5]
--     0 -> 6 [pos="e,405.43,418.85 390.49,458.77 394.9,446.99 400.87,431.02 405.32,419.13"]
--     12 [height=0.55967;pos="467.5,401.63";width=0.55967]
--     0 -> 12 [pos="e,452.8,415.41 397.83,463.19 412.75,450.22 436.85,429.27 452.43,415.73"]
--     2 [pos="330.5,325.33";width=0.5]
--     1 -> 2 [pos="e,336.35,342.42 351.64,384.51 347.15,372.14 340.97,355.15 336.45,342.72"]
--     7 [pos="384.5,325.33";width=0.5]
--     1 -> 7 [pos="e,378.65,342.42 363.36,384.51 367.85,372.14 374.03,355.15 378.54,342.72"]
--     13 [height=0.55967;pos="440.5,325.33";width=0.55967]
--     1 -> 13 [pos="e,425.95,339.36 370.47,389.02 385.4,375.66 409.87,353.75 425.58,339.69"]
--     3 [pos="263.5,249.04";width=0.5]
--     2 -> 3 [pos="e,275.26,263.08 318.83,311.39 306.7,297.94 287.81,277 275.55,263.4"]
--     8 [pos="419.5,249.04";width=0.5]
--     2 -> 8 [pos="e,406.15,261.18 344.02,313.05 360.71,299.11 388.95,275.54 405.75,261.51"]
--     14 [height=0.55967;pos="475.5,249.04";width=0.55967]
--     2 -> 14 [pos="e,459.28,261.56 344.39,313.55 348.48,310.63 353.06,307.61 357.5,305.19 394.93,284.71 408.73,289.04 446.5,269.19 450.64,267.01 454.93,\\\n264.4 458.9,261.81"]
--     18 [height=0.55967;pos="239.5,96.445";width=0.55967]
--     2 -> 18 [pos="e,221.18,105.3 313.92,317.87 277.75,302.66 192.9,260.73 166.5,192.89 160,176.2 158.52,168.63 166.5,152.59 177.74,130.02 203.11,114.24 \\\n220.76,105.5"]
--     19 [height=0.55967;pos="335.5,96.445";width=0.55967]
--     2 -> 19 [pos="e,335.94,116.84 331.52,307.33 332.98,282.28 335.56,234 336.5,192.89 336.91,174.98 336.66,170.5 336.5,152.59 336.39,140.81 336.16,\\\n127.62 335.94,117.09"]
--     4 [pos="101.5,172.74";width=0.5]
--     3 -> 4 [pos="e,117.56,181.11 247.37,240.64 216.44,226.46 149.09,195.57 117.92,181.27"]
--     9 [pos="193.5,172.74";width=0.5]
--     3 -> 9 [pos="e,205.67,186.66 251.62,235.43 238.93,221.96 218.89,200.69 205.97,186.98"]
--     15 [height=0.55967;pos="287.5,172.74";width=0.55967]
--     3 -> 15 [pos="e,281.6,192.01 268.82,231.55 272.58,219.93 277.61,204.35 281.51,192.29"]
--     5 [pos="48.498,96.445";width=0.5]
--     4 -> 5 [pos="e,58.506,111.47 91.279,157.42 81.908,144.28 68.101,124.92 58.727,111.78"]
--     10 [height=0.55967;pos="104.5,96.445";width=0.55967]
--     4 -> 10 [pos="e,103.72,116.67 102.19,154.51 102.65,143.28 103.24,128.61 103.71,116.95"]
--     16 [height=0.55967;pos="162.5,96.445";width=0.55967]
--     4 -> 16 [pos="e,150.12,112.52 112.69,158.11 123.2,145.31 138.91,126.18 149.86,112.83"]
--     5 -> 0 [pos="e,366.23,475.02 49.439,114.6 50.887,142.55 53.498,199.64 53.498,248.04 53.498,326.33 53.498,326.33 53.498,326.33 53.498,464.3 295.89,\\\n474.85 365.82,475.02"]
--     11 [height=0.54162;pos="19.498,20.148";width=0.54162]
--     5 -> 11 [pos="e,26.284,38.534 42.206,79.323 37.545,67.382 31.197,51.119 26.397,38.823"]
--     17 [height=0.55967;pos="77.498,20.148";width=0.55967]
--     5 -> 17 [pos="e,70.506,39.061 54.791,79.323 59.386,67.552 65.62,51.579 70.394,39.349"]
--     15 -> 18 [pos="e,250.12,113.89 276.85,155.25 268.95,143.04 258.24,126.45 250.31,114.18"]
--     15 -> 19 [pos="e,324.88,113.89 298.15,155.25 306.04,143.04 316.76,126.45 324.69,114.18"]
--     18 -> 3 [pos="e,260.79,231.06 242.53,116.49 247.24,145.99 256.21,202.31 260.74,230.72"]
--     19 -> 3 [pos="e,277.53,237.38 334.82,116.79 333.41,136.85 329.16,168.62 316.5,192.89 307.11,210.88 290.05,227.04 277.82,237.15"]
--     }
processGraph :: Graph -> IO Graph
processGraph g =
  runDotParser <$> dotB (directed g) (dotPrint g)

-- |
-- >>> exG1' <- processGraph exG1
-- >>> bb exG1'
-- Just Rect 0.0 495.65 0.0 493.78
bb :: Graph -> Maybe (Rect Double)
bb g = case runParser rectP . packUTF8 <$> v of
  Just (OK r _) -> Just r
  _ -> Nothing
  where
    v = case Map.lookup (IDString "bb") (attributes g) of
      (Just (IDQuoted q)) -> Just q
      _ -> Nothing

-- attributes :: AttributeType -> Graph -> Map.Map ID ID
attributes :: Graph -> Map.Map ID ID
attributes g = Map.fromList
  [(x,y) | (IDStatement x y) <- ls]
  where
    ls = mconcat $ toList . aList <$> mconcat [xs |(StatementAttribute (AttributeStatement GraphType (AttrList xs))) <- view #statements g]

-- |
-- Ignores 'Port' information
nodesG :: Graph -> Map.Map ID (Map.Map ID ID)
nodesG g =
  Map.fromList $
  [(x, atts a) |
   (StatementNode (NodeStatement (NodeID x _) a)) <- view #statements g]

atts :: AttrList -> Map.Map ID ID
atts a = Map.fromList $ (\(IDStatement x y) -> (x,y)) <$> mconcat (toList . aList <$> view #attrList a)

edgesG :: Graph -> Map.Map (ID, ID) (Map.Map ID ID)
edgesG g =
  Map.fromList $
  mconcat $ fmap (\(xs, a) -> (,a) <$> xs)
  [(edgePairs e, atts $ view #edgeStatementAttributes e) |
   (StatementEdge e) <- view #statements g]

edgeID2ID :: EdgeID -> Maybe ID
edgeID2ID x = case view #unedgeID x of
  Left (NodeID x' _) -> Just x'
  _ -> Nothing

edgeStatement2IDs :: EdgeStatement -> [(Maybe ID,Maybe ID)]
edgeStatement2IDs e = zip (id0:id1) id1
  where
    id0 = edgeID2ID (view #edgeStatementID e)
    id1 = toList $ edgeID2ID . view #edgeID <$> view (#edgeStatementRHS % #edgeRHSs) e

edgePairs :: EdgeStatement -> [(ID, ID)]
edgePairs e = [(x,y) | (Just x, Just y) <- edgeStatement2IDs e]

nodeA :: Graph -> ID -> Map.Map ID (Maybe ID)
nodeA g a = fmap (Map.lookup a) (nodesG g)

edgeA :: Graph -> ID -> Map.Map (ID,ID) (Maybe ID)
edgeA g a = fmap (Map.lookup a) (edgesG g)


gInt1 :: G.Graph Int
gInt1 = G.edges $
    [(v, (v + 1) `mod` 6) | v <- [0 .. 5]]
        ++ [(v, v + k) | v <- [0 .. 5], k <- [6, 12]]
        ++ [(2, 18), (2, 19), (15, 18), (15, 19), (18, 3), (19, 3)]

exG1 :: Graph
exG1 = defaultGraph & #statements %~ (<> toStatementsInt gInt1)

exG1' :: Graph
exG1' = unsafePerformIO $ processGraph exG1
{-# NOINLINE exG1' #-}

nodePos :: Graph -> Map.Map ID (Maybe (Point Double))
nodePos g =
  fmap (\x -> case x of
           Just (IDQuoted x') -> Just (runParser_ pointP (packUTF8 x'))
           _ -> Nothing) $
  nodeA g (IDString "pos")

nodeWidth :: Graph -> Map.Map ID (Maybe Double)
nodeWidth g =
  fmap (\x -> case x of
           Just (IDDouble x') -> Just x'
           _ -> Nothing) $
  nodeA g (IDString "width")

edgeWidth :: Graph -> Map.Map (ID, ID) (Maybe Double)
edgeWidth g =
  fmap (\x -> case x of
           Just (IDDouble x') -> Just x'
           _ -> Nothing) $
  edgeA g (IDString "width")

edgeCurve :: Graph -> Map.Map (ID, ID) (Maybe [Point Double])
edgeCurve g =
  fmap (\x -> case x of
           Just (IDQuoted x') -> Just (runParser_ curveP (packUTF8 x'))
           _ -> Nothing) $
  edgeA g (IDString "pos")

data NodeInfo = NodeInfo { nlabel :: Text, nwidth :: Double, pos :: Point Double } deriving (Eq, Show, Generic)

nodeInfo :: Graph -> Double -> [NodeInfo]
nodeInfo g w = [NodeInfo (Text.pack $ label x) (fromMaybe w (join w')) p | (x, (Just p, w')) <- xs]
  where
    xs = Map.toList $
         merge
         (mapMissing (\_ v -> (v,Nothing)))
         dropMissing
         (zipWithMatched (\_ x y -> (x,Just y)))
         (nodePos g)
         (nodeWidth g)

data EdgeInfo = EdgeInfo { ewidth :: Double, curve :: [PathData Double] } deriving (Eq, Show, Generic)

edgeInfo :: Graph -> Double -> [EdgeInfo]
edgeInfo g w = [EdgeInfo (fromMaybe w (join w')) (toPathDataE p) | ((_, _), (Just p, w')) <- xs]
  where
    xs = Map.toList $
         merge
         (mapMissing (\_ v -> (v,Nothing)))
         dropMissing
         (zipWithMatched (\_ x y -> (x,Just y)))
         (edgeCurve g)
         (edgeWidth g)


chunksOf :: Int -> [e] -> [[e]]
chunksOf _ [] = [[]]
chunksOf n xs = [Prelude.take n xs] <> chunksOf n (Prelude.drop n xs)

-- |
--
-- https://graphviz.org/docs/attr-types/splineType/
-- format of the example is end point point and then triples (5,8,11 lengths are 1, 2 and 3 cubics)
--
toPathDataE :: [Point Double] -> [PathData Double]
toPathDataE [] = []
toPathDataE (e:h:xs) = [StartP h] <> catMaybes (cubic <$> chunksOf 3 xs) <> [LineP e]
  where
    cubic [x,y,z] = Just (CubicP x y z)
    cubic _ = Nothing
toPathDataE _ = []

-- | convert a (processed) 'Graph' to a 'ChartSvg'
--
-- >>> writeChartSvg "exg1.svg" (graphToChart exG1')
graphToChart :: Graph -> ChartSvg
graphToChart g =
  mempty
    & #charts .~ unnamed (ps <> c0 <> [ts])
    & #svgOptions % #svgHeight .~ 500
    & #hudOptions .~ (mempty & #chartAspect .~ ChartAspect)
  where
    vshift' = -3.7
    -- node information
    ns = nodeInfo g 0.5
    -- edge information
    es = edgeInfo g 0.5
    -- paths
    ps = fmap (\(EdgeInfo w p) -> PathChart (defaultPathStyle & #borderSize .~ w & #borderColor .~ black & #color .~ transparent) p) es
    -- circles
    c0 = fmap (\(NodeInfo _ w p) -> GlyphChart (defaultGlyphStyle & #shape .~ CircleGlyph & #size .~ 72 * w & #borderSize .~ 0.5 & #borderColor .~ black & #color .~ transparent) [p]) ns
    -- labels
    ts =
      TextChart (defaultTextStyle & #size .~ 14) ((\(NodeInfo l _ (Point x y)) -> (l,Point x (vshift' + y))) <$> ns)

data Class
  = Magma
  | Unital
  | Associative
  | Commutative
  | Invertible
  | Idempotent
  | Absorbing
  | Group
  | AbelianGroup
  | Additive
  | Subtractive
  | Multiplicative
  | Divisive
  | Distributive
  | Semiring
  | Ring
  | IntegralDomain
  | Field
  | ExpField
  | QuotientField
  | UpperBoundedField
  | LowerBoundedField
  | TrigField
  | -- Higher-kinded numbers
    AdditiveAction
  | SubtractiveAction
  | MultiplicativeAction
  | DivisiveAction
  | Module
  | -- Lattice
    JoinSemiLattice
  | MeetSemiLattice
  | Lattice
  | BoundedJoinSemiLattice
  | BoundedMeetSemiLattice
  | BoundedLattice
  | -- Number Types
    Integral
  | Ratio
  | -- Measure
    Signed
  | Norm
  | Basis
  | Direction
  | Epsilon
  deriving (Show, Eq, Ord)

data Cluster
  = GroupCluster
  | LatticeCluster
  | RingCluster
  | FieldCluster
  | HigherKindedCluster
  | MeasureCluster
  | NumHaskCluster
  deriving (Show, Eq, Ord)

clusters :: Map.Map Class Cluster
clusters =
  Map.fromList
    [ (Magma, GroupCluster),
      (Unital, GroupCluster),
      (Associative, GroupCluster),
      (Commutative, GroupCluster),
      (Invertible, GroupCluster),
      (Idempotent, GroupCluster),
      (Absorbing, GroupCluster),
      (Group, GroupCluster),
      (AbelianGroup, GroupCluster),
      (Additive, NumHaskCluster),
      (Subtractive, NumHaskCluster),
      (Multiplicative, NumHaskCluster),
      (Divisive, NumHaskCluster),
      (Distributive, NumHaskCluster),
      (Semiring, NumHaskCluster),
      (Ring, NumHaskCluster),
      (IntegralDomain, NumHaskCluster),
      (Field, NumHaskCluster),
      (ExpField, FieldCluster),
      (QuotientField, FieldCluster),
      (UpperBoundedField, FieldCluster),
      (LowerBoundedField, FieldCluster),
      (TrigField, FieldCluster),
      (AdditiveAction, HigherKindedCluster),
      (SubtractiveAction, HigherKindedCluster),
      (MultiplicativeAction, NumHaskCluster),
      (DivisiveAction, HigherKindedCluster),
      (Module, NumHaskCluster),
      (JoinSemiLattice, LatticeCluster),
      (MeetSemiLattice, LatticeCluster),
      (Lattice, LatticeCluster),
      (BoundedJoinSemiLattice, LatticeCluster),
      (BoundedMeetSemiLattice, LatticeCluster),
      (BoundedLattice, LatticeCluster),
      (Norm, RingCluster),
      (Basis, RingCluster),
      (Direction, RingCluster),
      (Signed, RingCluster),
      (Epsilon, MeasureCluster),
      (Integral, RingCluster),
      (Ratio, FieldCluster)
    ]

data Family
  = Addition
  | Multiplication
  | Actor
  deriving (Show, Eq, Ord)

data Dependency = Dependency
  { _class :: Class,
    _dep :: Class,
    _op :: Maybe Family
  }
  deriving (Show, Eq, Ord)

dependencies :: [Dependency]
dependencies =
  [ Dependency Unital Magma Nothing,
    Dependency Associative Magma Nothing,
    Dependency Commutative Magma Nothing,
    Dependency Invertible Magma Nothing,
    Dependency Idempotent Magma Nothing,
    Dependency Absorbing Magma Nothing,
    Dependency Group Unital Nothing,
    Dependency Group Invertible Nothing,
    Dependency Group Associative Nothing,
    Dependency AbelianGroup Unital Nothing,
    Dependency AbelianGroup Invertible Nothing,
    Dependency AbelianGroup Associative Nothing,
    Dependency AbelianGroup Commutative Nothing,
    Dependency Additive Commutative (Just Addition),
    Dependency Additive Unital (Just Addition),
    Dependency Additive Associative (Just Addition),
    Dependency Subtractive Invertible (Just Addition),
    Dependency Subtractive Additive (Just Addition),
    Dependency Multiplicative Unital (Just Multiplication),
    Dependency Multiplicative Associative (Just Multiplication),
    Dependency Multiplicative Commutative (Just Multiplication),
    Dependency Divisive Invertible (Just Multiplication),
    Dependency Divisive Multiplicative (Just Multiplication),
    Dependency Distributive Additive (Just Addition),
    Dependency Distributive Multiplicative (Just Multiplication),
    Dependency Distributive Absorbing Nothing,
    Dependency Ring Distributive Nothing,
    Dependency Ring Subtractive (Just Addition),
    Dependency IntegralDomain Ring Nothing,
    Dependency Field Ring Nothing,
    Dependency Field Divisive (Just Multiplication),
    Dependency ExpField Field Nothing,
    Dependency QuotientField Field Nothing,
    Dependency QuotientField Ring Nothing,
    Dependency TrigField Field Nothing,
    Dependency UpperBoundedField Field Nothing,
    Dependency LowerBoundedField Field Nothing,
    -- higher-kinded relationships
    Dependency AdditiveAction Additive (Just Actor),
    Dependency SubtractiveAction Subtractive (Just Actor),
    Dependency MultiplicativeAction Multiplicative (Just Actor),
    Dependency DivisiveAction Divisive (Just Actor),
    Dependency Module Distributive (Just Actor),
    Dependency Module MultiplicativeAction Nothing,
    -- Lattice
    Dependency JoinSemiLattice Associative Nothing,
    Dependency JoinSemiLattice Commutative Nothing,
    Dependency JoinSemiLattice Idempotent Nothing,
    Dependency MeetSemiLattice Associative Nothing,
    Dependency MeetSemiLattice Commutative Nothing,
    Dependency MeetSemiLattice Idempotent Nothing,
    Dependency Lattice JoinSemiLattice Nothing,
    Dependency Lattice MeetSemiLattice Nothing,
    Dependency BoundedJoinSemiLattice JoinSemiLattice Nothing,
    Dependency BoundedJoinSemiLattice Unital Nothing,
    Dependency BoundedMeetSemiLattice MeetSemiLattice Nothing,
    Dependency BoundedMeetSemiLattice Unital Nothing,
    Dependency BoundedLattice BoundedJoinSemiLattice Nothing,
    Dependency BoundedLattice BoundedMeetSemiLattice Nothing,
    Dependency Signed Ring Nothing,
    Dependency Norm Ring Nothing,
    Dependency Basis Ring Nothing,
    Dependency Direction Ring Nothing,
    Dependency Epsilon Subtractive Nothing,
    Dependency Epsilon MeetSemiLattice Nothing,
    Dependency Integral Ring Nothing,
    Dependency Ratio Field Nothing
  ]

magmaClasses :: [Class]
magmaClasses =
  [ Magma,
    Unital,
    Associative,
    Commutative,
    Invertible,
    Absorbing,
    Additive,
    Subtractive,
    Multiplicative,
    Divisive,
    Distributive,
    Ring,
    Field
  ]

classesNH :: [Class]
classesNH =
  [ Additive,
    Subtractive,
    Multiplicative,
    Divisive,
    Distributive,
    Ring,
    Field,
    ExpField,
    QuotientField,
    TrigField,
    Signed,
    Norm,
    Basis,
    Direction,
    MultiplicativeAction,
    Module,
    UpperBoundedField,
    LowerBoundedField,
    Integral,
    Ratio
  ]

graphNH :: L.Graph (First Family) Class
graphNH =
  L.edges ((\(Dependency x y l) -> (First l,x,y)) <$> dependencies) <>
  L.vertices classesNH

fromFamily :: First Family -> Colour
fromFamily (First f) = case f of
  Nothing -> palette1 0
  Just Addition -> palette1 1
  Just Multiplication -> palette1 2
  Just Actor -> palette1 3

toStatementsShow :: (Show a, Ord a, Monoid e, Eq e) => L.Graph e a -> [Statement]
toStatementsShow g =
  ((\x -> StatementNode $ NodeStatement (NodeID (IDQuoted (show x)) Nothing) (AttrList [])) <$> L.vertexList g) <>
  ((\(_, x, y) ->
      StatementEdge $
      EdgeStatement
      (EdgeID (Left (NodeID (IDQuoted (show x)) Nothing)))
      (EdgeRHSs $ fromList [EdgeRHS EdgeDirected (EdgeID (Left (NodeID (IDQuoted (show y)) Nothing)))])
      (AttrList [])) <$> L.edgeList g)


dotGraphNH :: Graph
dotGraphNH = defaultGraph & #statements %~ (<> toStatementsShow graphNH)

dotGraphNH' :: Graph
dotGraphNH' = unsafePerformIO $ processGraph dotGraphNH
{-# NOINLINE dotGraphNH' #-}


-- >>> writeChartSvg "other/nh.svg" (graphToChartL dotGraphNH')
graphToChartL :: Graph -> ChartSvg
graphToChartL g =
  mempty
    & #charts .~ unnamed (ps <> c0 <> [ts])
    & #svgOptions % #svgHeight .~ 500
    & #hudOptions .~ (mempty & #chartAspect .~ ChartAspect)
  where
    vshift' = -3.7
    -- node information
    ns = nodeInfo g 0.5
    -- edge information
    es = edgeInfo g 0.5
    -- paths
    ps = fmap (\(EdgeInfo w p) -> PathChart (defaultPathStyle & #borderSize .~ w & #borderColor .~ black & #color .~ transparent) p) es
    -- circles
    c0 = fmap (\(NodeInfo _ w p) -> GlyphChart (defaultGlyphStyle & #shape .~ CircleGlyph & #size .~ 72 * w & #borderSize .~ 0.5 & #borderColor .~ black & #color .~ transparent) [p]) ns
    -- labels
    ts =
      TextChart (defaultTextStyle & #size .~ 14) ((\(NodeInfo l _ (Point x y)) -> (l,Point x (vshift' + y))) <$> ns)
