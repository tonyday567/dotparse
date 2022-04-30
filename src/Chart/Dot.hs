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

-- import Data.Text (Text)
import FlatParse.Basic
import Chart.Dot.TH
import Data.Char hiding (isDigit)
-- import Data.ByteString hiding (empty)
import qualified Data.ByteString.Char8 as C
import GHC.Generics
import NeatInterpolation
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString hiding (empty)
import Data.Maybe
import Data.Proxy
-- import Data.Text hiding (empty)
import Data.List.NonEmpty

-- $setup
-- >>> import Chart.Dot
-- >>> import FlatParse.Basic
-- >>> import NeatInterpolation
-- >>> import Data.Text.Encoding (encodeUtf8)
-- >>> :set -XOverloadedStrings

-- | run a dotParse erroring on leftovers, Fail or Err
parse_ :: (DotParse a) => ByteString -> a
parse_ b = case runParser dotParse b of
  OK r "" -> r
  OK _ x -> error $ unpackUTF8 $ "leftovers: " <> x
  Fail -> error "Fail"
  Err e -> error $ "Error: " <> e

-- * printing

class DotParse a where
  dotPrint :: a -> ByteString
  dotParse :: Parser e a

-- * Dot Grammar


-- | MergeEdges (strict)
--
-- >>> trip MergeEdges
-- True
--
-- >>> trip NoMergeEdges
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
btrip _ b = [b] == (dotPrint <$> [r | (OK r "") <- [runParser dotParse b :: Result () a]])

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
-- >>> runParser idP "apple_1'"
-- OK (IDString "apple_1") "'"
--
-- FIXME:
-- >>> :set -XQuasiQuotes
-- >>> runParser idP $ encodeUtf8 [trimming|"quoted \""|]
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
      (Data.ByteString.intercalate "\\\"" .
       Data.ByteString.split (toEnum $ fromEnum '"') .
       packUTF8) x
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
    dotPrint (IDStatement x0 x1) = dotPrint x0 <> " = " <> dotPrint x1

    dotParse = token $
      do
        x0 <- dotParse
        _ <- $(symbol "=")
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


data Port = Port { portID :: Maybe ID, portCompass :: Maybe Compass } deriving (Eq, Show, Generic)

instance DotParse Port
  where
    dotPrint (Port x0 c) = intercalate " " $ catMaybes [(": " <>) . dotPrint <$> x0, (": " <>) . dotPrint <$> c]

    dotParse = token $ Port <$> optional dotParse <*> optional dotParse

data NodeID = NodeID { nodeID' :: ID, nodePort :: Maybe Port } deriving (Eq, Show, Generic)

instance DotParse NodeID
  where
    dotPrint (NodeID x0 p) = intercalate " " $ catMaybes [Just . dotPrint $ x0, dotPrint <$> p]

    dotParse = token $ NodeID <$> dotParse <*> optional dotParse

-- |
-- >>> parse_ "shape=diamond; color=blue" :: AList
-- AList {aListID = IDStatement {firstID = IDString "shape", secondID = IDString "diamond"}, nextAList = Just (AList {aListID = IDStatement {firstID = IDString "color", secondID = IDString "blue"}, nextAList = Nothing})}
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
--
newtype AttrList = AttrList { attrList :: [AList] } deriving (Eq, Show, Generic)

instance DotParse AttrList
  where
    dotPrint (AttrList xs) = case xs of
      [] -> "[]"
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
data NodeStatement = NodeStatement { nodeID :: NodeID, nodeAttributes :: AttrList } deriving (Eq, Show, Generic)

instance DotParse NodeStatement
  where
    dotPrint (NodeStatement i l) = intercalate " " [dotPrint i, dotPrint l]

    dotParse = NodeStatement <$> dotParse <*> dotParse

-- |
newtype EdgeID = EdgeID { unedgeID :: Either NodeID SubGraphStatement} deriving (Eq, Show, Generic)

instance DotParse EdgeID
  where
    dotPrint (EdgeID e) = either dotPrint undefined e

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
                   "," -> pure EdgeUndirected
                |])

-- |
-- >>> parse_ "-> B" :: EdgeRHS
data EdgeRHS = EdgeRHS { edgeOp :: EdgeOp, edgeID :: EdgeID } deriving (Eq, Show, Generic)

instance DotParse EdgeRHS
  where
    dotPrint (EdgeRHS o e) = intercalate " " [dotPrint o, dotPrint e]

    dotParse = token $
      EdgeRHS <$> dotParse <*> dotParse

-- |
-- >>> parse_ "-> B -> C" :: EdgeRHSs
newtype EdgeRHSs = EdgeRHSs { edgeRHSs :: NonEmpty EdgeRHS } deriving (Eq, Show, Generic)

instance DotParse EdgeRHSs
  where
    dotPrint (EdgeRHSs xs) = intercalate " " (dotPrint <$> toList xs)

    dotParse = token $
      (\x0 x1 -> EdgeRHSs (x0:|x1)) <$> dotParse <*> many dotParse

-- |
-- >>> parse_ "A -> B [style=dashed, color=grey]"
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

    dotParse = token $
      (StatementNode <$> dotParse) <|>
      (StatementEdge <$> dotParse) <|>
      (StatementAttribute <$> dotParse) <|>
      (StatementID <$> dotParse) <|>
      (StatementSubGraph <$> dotParse)

-- each subgraph must have a unique name
data SubGraphStatement = SubGraphStatement { subgraphID :: Maybe ID, subgraphStatements :: [Statement] } deriving (Eq, Show, Generic)

instance DotParse SubGraphStatement
  where
    dotPrint (SubGraphStatement x xs) =
      intercalate " " $
      maybe []
      (\x' -> [intercalate " " ["subgraph", dotPrint x']]) x <>
      (:[]) (intercalate "/n" $ dotPrint <$> xs)

    dotParse = token $ do
      x <- optional ($(keyword "subgraph") *> dotParse)
      pure (SubGraphStatement x) <*> wrapCurlyP (many dotParse)

data Graph = Graph { mergeEdges :: Maybe MergeEdges, directed :: Directed, graphid :: Maybe ID, statements :: [Statement]  } deriving (Eq, Show, Generic)

instance DotParse Graph
  where
    dotPrint (Graph me d x xs) =
      intercalate " " $ maybe [] ((:[]) . dotPrint) me <> [dotPrint d] <> maybe [] ((:[]) . dotPrint) x <> [wrapCurlyPrint (intercalate "/n" (dotPrint <$> xs))]

    dotParse = token $
      Graph <$>
      optional dotParse <*>
      dotParse <*>
      optional dotParse <*>
      wrapCurlyP (many dotParse)

-- * parsing
digit :: Parser e Int
digit = (\c -> ord c - ord '0') <$> satisfyASCII isDigit

int :: Parser e Int
int = token do
  (place, n) <- chainr (\n (!place, !acc) -> (place*10,acc+place*n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser e (Int, Int)
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
double :: Parser e Double
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

quoted :: Parser e String
quoted =
  $(char '"') *> many unquoteQuote <* $(char '"')

unquoteQuote :: Parser e Char
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

wrapSquareP :: Parser e a -> Parser e a
wrapSquareP p =
  $(symbol "[") *> p <* $(symbol "]")

wrapSquarePrint :: ByteString -> ByteString
wrapSquarePrint b = "[ " <> b <> " ]"

wrapCurlyP :: Parser e a -> Parser e a
wrapCurlyP p = $(symbol "{") *> p <* $(symbol "}")

wrapCurlyPrint :: ByteString -> ByteString
wrapCurlyPrint b = "{ " <> b <> " }"

-- * examples

-- | minimal definition
-- >>> runParser graphP ex0
-- OK (Graph {mergeEdges = Just NoMergeMultiEdges, directed = UnDirected, graphid = Nothing, statements = []}) ""
ex0 :: ByteString
ex0 = encodeUtf8 [trimming|
graph {
}
|]

-- | Examples from https://renenyffenegger.ch/notes/tools/Graphviz/examples/index
--
-- FIXME:
-- Fails on ex3 to 3x9, ex14, ex15
--
-- >>> runParser graphP ex1
-- OK (Graph {mergeEdges = Just NoMergeMultiEdges, directed = Directed, graphid = Just (IDString "D"), statements = [StatementNode (NodeStatement {nodeID = NodeID {nodeID' = IDString "A", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}, nodeAttributes = AttrList {aList = Just (AList {aListID = IDStatement {firstID = IDString "shape", secondID = IDString "diamond"}, nextAList = Nothing}), attrList = Nothing}}),StatementNode (NodeStatement {nodeID = NodeID {nodeID' = IDString "B", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}, nodeAttributes = AttrList {aList = Just (AList {aListID = IDStatement {firstID = IDString "shape", secondID = IDString "box"}, nextAList = Nothing}), attrList = Nothing}}),StatementNode (NodeStatement {nodeID = NodeID {nodeID' = IDString "C", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}, nodeAttributes = AttrList {aList = Just (AList {aListID = IDStatement {firstID = IDString "shape", secondID = IDString "circle"}, nextAList = Nothing}), attrList = Nothing}}),StatementEdge (EdgeStatement {edgeStatementID = Left (NodeID {nodeID' = IDString "A", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}), edgeStatementRHS = EdgeRHS {edgeID = Left (NodeID {nodeID' = IDString "B", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}), edgeRHSs = []}, edgeStatementAttributes = AttrList {aList = Just (AList {aListID = IDStatement {firstID = IDString "style", secondID = IDString "dashed"}, nextAList = Just (AList {aListID = IDStatement {firstID = IDString "color", secondID = IDString "grey"}, nextAList = Nothing})}), attrList = Nothing}}),StatementEdge (EdgeStatement {edgeStatementID = Left (NodeID {nodeID' = IDString "A", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}), edgeStatementRHS = EdgeRHS {edgeID = Left (NodeID {nodeID' = IDString "C", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}), edgeRHSs = []}, edgeStatementAttributes = AttrList {aList = Just (AList {aListID = IDStatement {firstID = IDString "color", secondID = IDQuoted "black:invis:black"}, nextAList = Nothing}), attrList = Nothing}}),StatementEdge (EdgeStatement {edgeStatementID = Left (NodeID {nodeID' = IDString "A", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}), edgeStatementRHS = EdgeRHS {edgeID = Left (NodeID {nodeID' = IDString "D", nodePort = Just (Port {portID = Nothing, portCompass = Nothing})}), edgeRHSs = []}, edgeStatementAttributes = AttrList {aList = Just (AList {aListID = IDStatement {firstID = IDString "penwidth", secondID = IDInt 5}, nextAList = Just (AList {aListID = IDStatement {firstID = IDString "arrowhead", secondID = IDString "none"}, nextAList = Nothing})}), attrList = Nothing}})]}) ""
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

ex2 :: ByteString
ex2 = encodeUtf8 [trimming|
digraph D {

    node [fontname="Arial"];

    node_A [shape=record    label="shape=record|{above|middle|below}|right"];
    node_B [shape=plaintext label="shape=plaintext|{curly|braces and|bars without}|effect"];

}
|]


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
