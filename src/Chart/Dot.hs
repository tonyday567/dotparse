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

-- | Abstract Grammar from:
-- http://www.graphviz.org/doc/info/lang.html

module Chart.Dot where

import FlatParse.Basic hiding (cut, lines)
import Chart.Dot.TH
import Data.Char hiding (isDigit)
import qualified Data.ByteString.Char8 as C
import GHC.Generics
import NeatInterpolation
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString hiding (zipWith, putStrLn, map, length, head, empty)
import Data.Proxy
import Data.List.NonEmpty hiding (zipWith, map, (!!), length, head)
import Data.Bool
import Data.These
import Prelude hiding (replicate)
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Algorithm.DiffOutput
import Data.Algorithm.Diff

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
parse_ :: (DotParse a) => ByteString -> a
parse_ b = case runParser dotParse b of
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
-- >>> parse_ "0" :: ID
-- IDInt 0
--
-- >>> parse_ "-.123" :: ID
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
-- >>> parse_ (encodeUtf8 [trimming|<The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>|]) :: ID
-- IDHtml "<The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>"
data ID = IDString String | IDInt Int | IDDouble Double | IDQuoted String | IDHtml String deriving (Eq, Show, Generic)

instance DotParse ID
  where
    dotPrint (IDString s) = packUTF8 s
    dotPrint (IDInt i) = packUTF8 (show i)
    dotPrint (IDDouble x) = packUTF8 (show x)
    dotPrint (IDQuoted x) =
      packUTF8 (show x)
    dotPrint (IDHtml s) = packUTF8 s

    dotParse =
      (IDString . C.unpack <$> ident) <|>
      (IDInt <$> signed int) <|>
      (IDDouble <$> signed double) <|>
      (IDQuoted <$> quoted) <|>
      (IDHtml <$> htmlLike)

-- | Attribute tuple
--
-- >>> parse_ "shape=diamond" :: IDStatement
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
-- >>> parse_ "shape=diamond; color=blue" :: AList
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
-- >>> parse_ "[shape=diamond; color=blue]" :: AttrList
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
-- >>> parse_ "A [shape=diamond; color=blue]" :: Statement
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
-- >>> parse_ "-> B" :: EdgeRHS
-- EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "B", nodePort = Nothing})}}
data EdgeRHS = EdgeRHS { edgeOp :: EdgeOp, edgeID :: EdgeID } deriving (Eq, Show, Generic)

instance DotParse EdgeRHS
  where
    dotPrint (EdgeRHS o e) = intercalate " " [dotPrint o, dotPrint e]

    dotParse = token $
      EdgeRHS <$> dotParse <*> dotParse

-- |
-- >>> parse_ "-> B -> C" :: EdgeRHSs
-- EdgeRHSs {edgeRHSs = EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "B", nodePort = Nothing})}} :| [EdgeRHS {edgeOp = EdgeDirected, edgeID = EdgeID {unedgeID = Left (NodeID {nodeID' = IDString "C", nodePort = Nothing})}}]}
newtype EdgeRHSs = EdgeRHSs { edgeRHSs :: NonEmpty EdgeRHS } deriving (Eq, Show, Generic)

instance DotParse EdgeRHSs
  where
    dotPrint (EdgeRHSs xs) = intercalate " " (dotPrint <$> toList xs)

    dotParse = token $
      (\x0 x1 -> EdgeRHSs (x0:|x1)) <$> dotParse <*> many dotParse

-- |
-- >>> parse_ "A -> B [style=dashed, color=grey]" :: EdgeStatement
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
      (StatementNode <$> dotParse) <|>
      (StatementAttribute <$> dotParse) <|>
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
int = (token do
  (place, n) <- chainr (\n (!place, !acc) -> (place*10,acc+place*n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n)

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
double = (token do
  (placel, nl) <- digits
  optioned ($(char '.') *> digits)
    (\(placer, nr) ->
       case (placel, placer) of
         (1,1) -> empty
         _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer)
    (case placel of
      1 -> empty
      _ -> pure $ fromIntegral nl))

-- |
-- >>> runParser (signed double) "-1.234x"
-- OK (-1.234) "x"
signed :: Num b => Parser e b -> Parser e b
signed p = optioned $(char '-') (const (((-1) *) <$> p)) p

quoted :: Parser Error String
quoted =
  $(symbol "\"") *> many unquoteQuote <* $(symbol' "\"")

unquoteQuote :: Parser Error Char
unquoteQuote = (do
  next <- satisfy (/= '"')
  case next of
    '/' -> branch (lookahead $(char '"')) ('"' <$ $(char '"')) (pure '/')
    x -> pure x)

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

-- * examples

-- | minimal definition
-- >>> parse_ ex0 :: Graph
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
testAll = sequence_ $ zipWith (>>)
  (putStrLn <$> ["ex0","ex1","ex2","ex3","ex4","ex5","ex6","ex7"
                ,"ex8","ex9","ex10","ex11","ex12","ex13","ex14","ex15"])
  (testParserP (Proxy :: Proxy Graph) <$>
  [ex0,ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,ex13,ex14,ex15])
