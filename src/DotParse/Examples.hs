{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Examples of conversion from dot ByteStrings
module DotParse.Examples where

import qualified Algebra.Graph as G
import Control.Monad
import Data.ByteString hiding (empty, head, length, map, zip, zipWith)
import Data.Proxy
import Data.Text.Encoding (encodeUtf8)
import DotParse
import Optics.Core
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> import Data.Proxy
-- >>> :set -XOverloadedStrings

-- * examples

-- | minimal definition
--
-- >>> runDotParser ex0 :: Graph
-- Graph {strict = Last {getLast = Just NoMergeEdges}, directed = Last {getLast = Just UnDirected}, graphid = Last {getLast = Nothing}, nodeAttributes = fromList [], graphAttributes = fromList [], edgeAttributes = fromList [], globalAttributes = fromList [], nodes = [], edges = [], subgraphs = []}
--
-- >>> testDotParser (Proxy :: Proxy Graph) defaultDotConfig ex0
ex0 :: ByteString
ex0 =
  encodeUtf8
    [trimming|
graph {}
|]

-- | Examples from https://renenyffenegger.ch/notes/tools/Graphviz/examples/index
--
-- >>> testDotParser (Proxy :: Proxy Graph) defaultDotConfig ex1
--
-- ![Example](other/ex1.svg)
ex1 :: ByteString
ex1 =
  encodeUtf8
    [trimming|
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
-- >>> testDotParser (Proxy :: Proxy Graph) defaultDotConfig ex2
--
-- ![Example](other/ex2.svg)
ex2 :: ByteString
ex2 =
  encodeUtf8
    [trimming|
digraph D {

    node [fontname="Arial"];

    node_A [shape=record    label="shape=record|{above|middle|below}|right"];
    node_B [shape=plaintext label="shape=plaintext|{curly|braces and|bars without}|effect"];

}
|]

-- |
-- >>> testDotParser (Proxy :: Proxy Graph) defaultDotConfig ex3
--
-- ![Example](other/ex3.svg)
ex3 :: ByteString
ex3 =
  encodeUtf8
    [trimming|
digraph D {
  A -> {B, C, D} -> {F}
}
|]

-- |
--
-- ![Example](other/ex4.svg)
ex4 :: ByteString
ex4 =
  encodeUtf8
    [trimming|
digraph L {

  node [shape=record fontname=Arial];

  a  [label="one\ltwo three\lfour five six seven\l"]
  b  [label="one\ntwo three\nfour five six seven"]
  c  [label="one\rtwo three\rfour five six seven\r"]

  a -> b -> c

}
|]

-- |
--
-- ![Example](other/ex5.svg)
ex5 :: ByteString
ex5 =
  encodeUtf8
    [trimming|
digraph D {

  label = "The foo, the bar and the baz";
  labelloc = "t"; // place the label at the top (b seems to be default)

  node [shape=plaintext]

  FOO -> {BAR, BAZ};


}
|]

-- |
--
-- ![Example](other/ex6.svg)
ex6 :: ByteString
ex6 =
  encodeUtf8
    [trimming|
digraph D {

  label = <The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>;
  labelloc = "t"; // place the label at the top (b seems to be default)

  node [shape=plaintext]

  FOO -> {BAR, BAZ};

}
|]

-- |
--
-- ![Example](other/ex7.svg)
ex7 :: ByteString
ex7 =
  encodeUtf8
    [trimming|
digraph R {

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

-- |
--
-- ![Example](other/ex8.svg)
ex8 :: ByteString
ex8 =
  encodeUtf8
    [trimming|
digraph Q {

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

-- |
--
-- ![Example](other/ex9.svg)
ex9 :: ByteString
ex9 =
  encodeUtf8
    [trimming|
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

-- |
--
-- ![Example](other/ex10.svg)
ex10 :: ByteString
ex10 =
  encodeUtf8
    [trimming|
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

-- |
--
-- ![Example](other/ex11.svg)
ex11 :: ByteString
ex11 =
  encodeUtf8
    [trimming|
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

-- |
--
-- ![Example](other/ex12.svg)
ex12 :: ByteString
ex12 =
  encodeUtf8
    [trimming|
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

-- |
--
-- ![Example](other/ex13.svg)
ex13 :: ByteString
ex13 =
  encodeUtf8
    [trimming|
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

-- |
--
-- ![Example](other/ex14.svg)
ex14 :: ByteString
ex14 =
  encodeUtf8
    [trimming|
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

-- |
--
-- ![Example](other/ex15.svg)
ex15 :: ByteString
ex15 =
  encodeUtf8
    [trimming|
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
exGInt :: G.Graph Int
exGInt =
  G.edges $
    [(v, (v + 1) `mod` 6) | v <- [0 .. 5]]
      ++ [(v, v + k) | v <- [0 .. 5], k <- [6, 12]]
      ++ [(2, 18), (2, 19), (15, 18), (15, 19), (18, 3), (19, 3)]

-- |
--
-- > exInt = defaultGraph & addStatements (toStatements Directed (strToUtf8 . show <$> exGInt))
-- > import qualified Data.ByteString.Char8 as B
-- > g <- processGraph exInt
-- > B.putStrLn $ dotPrint g
-- digraph {
--     graph [bb="0,0,495.65,493.78";overlap=false;size="1!";splines=spline]
--     node [height=0.5;label="\N";shape=circle]
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
--     2 -> 14 [pos="e,459.28,261.56 344.39,313.55 348.48,310.63 353.06,307.61 357.5,305.19 394.93,284.71 408.73,289.04 446.5,269.19 450.64,267.01 454.93,\
-- 264.4 458.9,261.81"]
--     18 [height=0.55967;pos="239.5,96.445";width=0.55967]
--     2 -> 18 [pos="e,221.18,105.3 313.92,317.87 277.75,302.66 192.9,260.73 166.5,192.89 160,176.2 158.52,168.63 166.5,152.59 177.74,130.02 203.11,114.24 \
-- 220.76,105.5"]
--     19 [height=0.55967;pos="335.5,96.445";width=0.55967]
--     2 -> 19 [pos="e,335.94,116.84 331.52,307.33 332.98,282.28 335.56,234 336.5,192.89 336.91,174.98 336.66,170.5 336.5,152.59 336.39,140.81 336.16,\
-- 127.62 335.94,117.09"]
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
--     5 -> 0 [pos="e,366.23,475.02 49.439,114.6 50.887,142.55 53.498,199.64 53.498,248.04 53.498,326.33 53.498,326.33 53.498,326.33 53.498,464.3 295.89,\
-- 474.85 365.82,475.02"]
--     11 [height=0.54162;pos="19.498,20.148";width=0.54162]
--     5 -> 11 [pos="e,26.284,38.534 42.206,79.323 37.545,67.382 31.197,51.119 26.397,38.823"]
--     17 [height=0.55967;pos="77.498,20.148";width=0.55967]
--     5 -> 17 [pos="e,70.506,39.061 54.791,79.323 59.386,67.552 65.62,51.579 70.394,39.349"]
--     15 -> 18 [pos="e,250.12,113.89 276.85,155.25 268.95,143.04 258.24,126.45 250.31,114.18"]
--     15 -> 19 [pos="e,324.88,113.89 298.15,155.25 306.04,143.04 316.76,126.45 324.69,114.18"]
--     18 -> 3 [pos="e,260.79,231.06 242.53,116.49 247.24,145.99 256.21,202.31 260.74,230.72"]
--     19 -> 3 [pos="e,277.53,237.38 334.82,116.79 333.41,136.85 329.16,168.62 316.5,192.89 307.11,210.88 290.05,227.04 277.82,237.15"]
--     }
exInt :: Graph
exInt = defaultGraph & addStatements (toStatements Directed (strToUtf8 . show <$> exGInt))
