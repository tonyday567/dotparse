{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Examples of conversion from dot ByteStrings
--
-- Most examples from https://renenyffenegger.ch/notes/tools/Graphviz/examples/index
module DotParse.Examples where

import Algebra.Graph qualified as G
import Control.Monad (zipWithM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Proxy (Proxy (..))
import DotParse
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> import Data.Proxy
-- >>> :set -XOverloadedStrings
-- >>> -- >>> import qualified Data.ByteString.Char8 as Char8
-- >>> import Optics.Core
-- >>> import Chart

-- * examples

-- | minimal definition
--
-- >>> runDotParser ex0 :: Graph
-- Graph {strict = Last {getLast = Just NoMergeEdges}, directed = Last {getLast = Just UnDirected}, graphid = Last {getLast = Nothing}, nodeAttributes = fromList [], graphAttributes = fromList [], edgeAttributes = fromList [], globalAttributes = fromList [], nodes = [], edges = [], subgraphs = []}
--
-- >>> testDotParser (Proxy :: Proxy Graph) defaultDotConfig ex0
ex0 :: ByteString
ex0 =
  C8.pack "graph {}\n"

-- |
-- ![Example](other/ex1.svg)
ex1 :: ByteString
ex1 =
  C8.pack "digraph D {\n    A [shape=diamond]\n    B [shape=box]\n    C [shape=circle]\n    A -> B [style=dashed, color=grey]\n    A -> C [color=\"black:invis:black\"]\n    A -> D [penwidth=5, arrowhead=none]\n    }\n"

-- |
-- ![Example](other/ex2.svg)
ex2 :: ByteString
ex2 =
  C8.pack "digraph D {\n\n    node [fontname=\"Arial\"];\n\n    node_A [shape=record    label=\"shape=record|{above|middle|below}|right\"];\n    node_B [shape=plaintext label=\"shape=plaintext|{curly|braces and|bars without}|effect\"];\n\n}\n"

-- |
-- ![Example](other/ex3.svg)
ex3 :: ByteString
ex3 =
  C8.pack "digraph D {\n  A -> {B, C, D} -> {F}\n}\n"

-- |
--
-- ![Example](other/ex4.svg)
ex4 :: ByteString
ex4 =
  C8.pack "digraph L {\n\n  node [shape=record fontname=Arial];\n\n  a  [label=\"one\\ntwo three\\nfour five six seven\\n\"]\n  b  [label=\"one\\ntwo three\\nfour five six seven\"]\n  c  [label=\"one\\rtwo three\\rfour five six seven\\r\"]\n\n  a -> b -> c\n\n}\n"

-- |
--
-- ![Example](other/ex5.svg)
ex5 :: ByteString
ex5 =
  C8.pack "digraph D {\n\n  label = \"The foo, the bar and the baz\";\n  labelloc = \"t\"; // place the label at the top (b seems to be default)\n\n  node [shape=plaintext]\n\n  FOO -> {BAR, BAZ};\n\n\n}\n"

-- |
--
-- ![Example](other/ex6.svg)
ex6 :: ByteString
ex6 =
  C8.pack "digraph D {\n\n  label = <The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>;\n  labelloc = \"t\"; // place the label at the top (b seems to be default)\n\n  node [shape=plaintext]\n\n  FOO -> {BAR, BAZ};\n\n}\n"

-- |
--
-- ![Example](other/ex7.svg)
ex7 :: ByteString
ex7 =
  C8.pack "digraph R {\n\n  { rank=same rA sA tA }\n  { rank=same uB vB wB }\n\n\n   rA -> sA;\n   sA -> vB;\n   t  -> rA;\n   uB -> vB;\n   wB -> u;\n   wB -> tA;\n\n}\n"

-- |
--
-- ![Example](other/ex8.svg)
ex8 :: ByteString
ex8 =
  C8.pack "digraph Q {\n\n  nd_1   [label = \"Node 1\"];\n  nd_2   [label = \"Node 2\"];\n  nd_3_a [label = \"Above Right Node 3\"];\n  nd_3_l [label = \"Left of Node 3\"];\n  nd_3   [label = \"Node 3\"];\n  nd_3_r [label = \"Right of Node 3\"];\n  nd_4   [label = \"Node 4\"];\n\n\n  nd_3_a -> nd_3_r;\n  nd_1 -> nd_2 -> nd_3 -> nd_4;\n\n  subgraph cluster_R {\n\n    {rank=same nd_3_l nd_3 nd_3_r}\n\n    nd_3_l -> nd_3 -> nd_3_r [color=grey arrowhead=none];\n\n  }\n\n}\n"

-- |
--
-- ![Example](other/ex9.svg)
ex9 :: ByteString
ex9 =
  C8.pack "digraph D {\n\n  subgraph cluster_p {\n    label = \"Parent\";\n\n    subgraph cluster_c1 {\n      label = \"Child one\";\n      a;\n\n      subgraph cluster_gc_1 {\n        label = \"Grand-Child one\";\n         b;\n      }\n      subgraph cluster_gc_2 {\n        label = \"Grand-Child two\";\n          c;\n          d;\n      }\n\n    }\n\n    subgraph cluster_c2 {\n      label = \"Child two\";\n      e;\n    }\n  }\n}\n"

-- |
--
-- ![Example](other/ex10.svg)
ex10 :: ByteString
ex10 =
  C8.pack "digraph H {\n\n  aHtmlTable [\n   shape=plaintext\n   color=blue      // The color of the border of the table\n   label=<\n\n     <table border='1' cellborder='0'>\n       <tr><td>col 1</td><td>foo</td></tr>\n       <tr><td>COL 2</td><td>bar</td></tr>\n     </table>\n\n  >];\n\n}"

-- |
--
-- ![Example](other/ex11.svg)
ex11 :: ByteString
ex11 =
  C8.pack "digraph {\n\n  tbl [\n\n    shape=plaintext\n    label=<\n\n      <table border='0' cellborder='1' color='blue' cellspacing='0'>\n        <tr><td>foo</td><td>bar</td><td>baz</td></tr>\n        <tr><td cellpadding='4'>\n          <table color='orange' cellspacing='0'>\n            <tr><td>one  </td><td>two  </td><td>three</td></tr>\n            <tr><td>four </td><td>five </td><td>six  </td></tr>\n            <tr><td>seven</td><td>eight</td><td>nine </td></tr>\n          </table>\n        </td>\n        <td colspan='2' rowspan='2'>\n          <table color='pink' border='0' cellborder='1' cellpadding='10' cellspacing='0'>\n            <tr><td>eins</td><td>zwei</td><td rowspan='2'>drei<br/>sechs</td></tr>\n            <tr><td>vier</td><td>fünf</td>                             </tr>\n          </table>\n        </td>\n        </tr>\n\n        <tr><td>abc</td></tr>\n\n      </table>\n\n    >];\n\n}\n"

-- |
--
-- ![Example](other/ex12.svg)
ex12 :: ByteString
ex12 =
  C8.pack "digraph D {\n\n  node [shape=plaintext]\n\n  some_node [\n   label=<\n     <table border=\"0\" cellborder=\"1\" cellspacing=\"0\">\n       <tr><td bgcolor=\"yellow\">Foo</td></tr>\n       <tr><td bgcolor=\"lightblue\"><font color=\"\\#0000ff\">Bar</font></td></tr>\n       <tr><td bgcolor=\"\\#f0e3ff\"><font color=\"\\#ff1020\">Baz</font></td></tr>\n     </table>>\n  ];\n}\n"

-- |
--
-- ![Example](other/ex13.svg)
ex13 :: ByteString
ex13 =
  C8.pack "digraph H {\n\n  aHtmlTable [\n   shape=plaintext\n   label=<\n\n     <table border='1' cellborder='0' style='rounded'>\n       <tr><td>col 1</td><td>foo</td></tr>\n       <tr><td>COL 2</td><td>bar</td></tr>\n     </table>\n\n  >];\n\n}\n"

-- |
--
-- ![Example](other/ex14.svg)
ex14 :: ByteString
ex14 =
  C8.pack "digraph H {\n\n  parent [\n   shape=plaintext\n   label=<\n     <table border='1' cellborder='1'>\n       <tr><td colspan=\"3\">The foo, the bar and the baz</td></tr>\n       <tr><td port='port_one'>First port</td><td port='port_two'>Second port</td><td port='port_three'>Third port</td></tr>\n     </table>\n  >];\n\n  child_one [\n   shape=plaintext\n   label=<\n     <table border='1' cellborder='0'>\n       <tr><td>1</td></tr>\n     </table>\n  >];\n\n  child_two [\n   shape=plaintext\n   label=<\n     <table border='1' cellborder='0'>\n       <tr><td>2</td></tr>\n     </table>\n  >];\n\n  child_three [\n   shape=plaintext\n   label=<\n     <table border='1' cellborder='0'>\n       <tr><td>3</td></tr>\n     </table>\n  >];\n\n  parent:port_one   -> child_one;\n  parent:port_two   -> child_two;\n  parent:port_three -> child_three;\n\n}\n"

-- |
--
-- ![Example](other/ex15.svg)
ex15 :: ByteString
ex15 =
  C8.pack "digraph D {\n\n  node [shape=plaintext fontname=\"Sans serif\" fontsize=\"8\"];\n\n  task_menu [ label=<\n   <table border=\"1\" cellborder=\"0\" cellspacing=\"1\">\n     <tr><td align=\"left\"><b>Task 1</b></td></tr>\n     <tr><td align=\"left\">Choose Menu</td></tr>\n     <tr><td align=\"left\"><font color=\"darkgreen\">done</font></td></tr>\n   </table>>];\n\n  task_ingredients [ label=<\n   <table border=\"1\" cellborder=\"0\" cellspacing=\"1\">\n     <tr><td align=\"left\"><b>Task 2</b></td></tr>\n     <tr><td align=\"left\">Buy ingredients</td></tr>\n     <tr><td align=\"left\"><font color=\"darkgreen\">done</font></td></tr>\n   </table>>];\n\n  task_invitation [ label=<\n   <table border=\"1\" cellborder=\"0\" cellspacing=\"1\">\n     <tr><td align=\"left\"><b>Task 4</b></td></tr>\n     <tr><td align=\"left\">Send invitation</td></tr>\n     <tr><td align=\"left\"><font color=\"darkgreen\">done</font></td></tr>\n   </table>>];\n\n  task_cook [ label=<\n   <table border=\"1\" cellborder=\"0\" cellspacing=\"1\">\n     <tr><td align=\"left\"><b>Task 5</b></td></tr>\n     <tr><td align=\"left\">Cook</td></tr>\n     <tr><td align=\"left\"><font color=\"red\">todo</font></td></tr>\n   </table>>];\n\n  task_table[ label=<\n   <table border=\"1\" cellborder=\"0\" cellspacing=\"1\">\n     <tr><td align=\"left\"><b>Task 3</b></td></tr>\n     <tr><td align=\"left\">Lay table</td></tr>\n     <tr><td align=\"left\"><font color=\"red\">todo</font></td></tr>\n   </table>>];\n\n  task_eat[ label=<\n   <table border=\"1\" cellborder=\"0\" cellspacing=\"1\">\n     <tr><td align=\"left\"><b>Task 6</b></td></tr>\n     <tr><td align=\"left\">Eat</td></tr>\n     <tr><td align=\"left\"><font color=\"red\">todo</font></td></tr>\n   </table>>];\n\n\n  task_menu        -> task_ingredients;\n  task_ingredients -> task_cook;\n  task_invitation  -> task_cook;\n  task_table       -> task_eat;\n  task_cook        -> task_eat;\n\n}\n"

-- | Test all the examples
testAll :: IO ()
testAll =
  zipWithM_
    (>>)
    ( putStrLn
        <$> [ "ex0",
              "ex1",
              "ex2",
              "ex3",
              "ex4",
              "ex5",
              "ex6",
              "ex7",
              "ex8",
              "ex9",
              "ex10",
              "ex11",
              "ex12",
              "ex13",
              "ex14",
              "ex15"
            ]
    )
    ( testDotParser (Proxy :: Proxy Graph) defaultDotConfig
        <$> [ex0, ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12, ex13, ex14, ex15]
    )

-- | Render all the examples
svgAll :: IO ()
svgAll =
  zipWithM_
    (>>)
    ( putStrLn
        <$> [ "ex0",
              "ex1",
              "ex2",
              "ex3",
              "ex4",
              "ex5",
              "ex6",
              "ex7",
              "ex8",
              "ex9",
              "ex10",
              "ex11",
              "ex12",
              "ex13",
              "ex14",
              "ex15"
            ]
    )
    ( zipWith
        (\b f -> processDotWith Directed ["-Tsvg", "-oother/" <> f <> ".svg"] b)
        [ex0, ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12, ex13, ex14, ex15]
        [ "ex0",
          "ex1",
          "ex2",
          "ex3",
          "ex4",
          "ex5",
          "ex6",
          "ex7",
          "ex8",
          "ex9",
          "ex10",
          "ex11",
          "ex12",
          "ex13",
          "ex14",
          "ex15"
        ]
    )

-- | algebraic graph example
--
-- > exGraph = defaultGraph & addStatements (toStatements Directed (Char8.pack . show <$> exAGraph))
-- > exGraphAugmented <- processGraph exGraph
-- > writeChartOptions "other/exga.svg" (graphToChart exGraphAugmented)
--
-- ![augmentation example](other/exga.svg)
exAGraph :: G.Graph Int
exAGraph =
  G.edges $
    [(v, (v + 1) `mod` 6) | v <- [0 .. 5]]
      <> [(v, v + k) | v <- [0 .. 5], k <- [6, 12]]
      <> [(2, 18), (2, 19), (15, 18), (15, 19), (18, 3), (19, 3)]
