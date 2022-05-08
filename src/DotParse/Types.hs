{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Abstract Grammar for the dot language.
-- http://www.graphviz.org/doc/info/lang.html

module DotParse.Types
 ( DotParse (..),
   testDotParser,
   runDotParser,

   Graph (..),
   gattL,
   attL,
   defaultGraph,
   processDotWith,
   processDot,
   processGraph,
   graphToChart,

   -- * components
   Strict (..),
   defStrict,
   Directed (..),
   defDirected,
   ID (..),
   label,
   Compass (..),
   Port (..),
   AttributeType (..),
   AttributeStatement (..),
   NodeStatement (..),
   EdgeID (..),
   EdgeOp (..),
   fromDirected,
   EdgeStatement (..),
   edgeID,
   edgeIDs,
   edgeIDsNamed,
   Statement (..),
   addStatement,
   addStatements,
   SubGraphStatement (..),

   -- * Graph Extraction
   bb,
   nodesPortL,
   nodesL,
   edgesL,
   nodesA,
   edgesA,
   nodePos,
   nodeWidth,
   edgeSpline,
   edgeWidth,
   nodeInfo,
   edgeInfo,
   splinePath,

   -- * Algebraic.Graph conversion
   toStatements,

 ) where

import FlatParse.Basic hiding (cut, lines)
import DotParse.FlatParse
import GHC.Generics
import NeatInterpolation
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString hiding (any, filter, zip, zipWith, putStrLn, map, length, head, empty)
import Data.Proxy
import Data.List.NonEmpty hiding (filter, zip, zipWith, map, (!!), length, head)
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
import Data.Text (Text)
import Chart
import Data.Maybe
import Data.Map.Merge.Strict
import qualified Data.Text as Text
import Data.Monoid

-- $setup
-- >>> import DotParse
-- >>> import DotParse.FlatParse
-- >>> import FlatParse.Basic
-- >>> import Data.Proxy
-- >>> import NeatInterpolation
-- >>> import Data.Text.Encoding (encodeUtf8)
-- >>> import qualified Data.Map as Map
-- >>> :set -XOverloadedStrings

-- * A parser & printer class for a graphviz graph and components of its dot language
class DotParse a where
  dotPrint :: a -> ByteString
  dotParse :: Parser Error a

-- | dotParse and then dotPrint:
--
-- - pretty printing error on failure.
--
-- - This is not an exact parser/printer, so the test re-parses the dotPrint, which should be idempotent
testDotParser :: forall a. (DotParse a) => Proxy a -> ByteString -> IO ()
testDotParser _ b =
  case runParser dotParse b :: Result Error a of
    Err e -> B.putStrLn $ prettyError b e
    OK a left -> do
      when (left /= "") (B.putStrLn $ "parsed with leftovers: " <> left)
      case runParser dotParse (dotPrint a) :: Result Error a of
        Err e -> B.putStrLn $ "round trip error: " <> prettyError (dotPrint a) e
        Fail -> B.putStrLn "uncaught round trip parse error"
        OK _ left' -> do
          when (left' /= "") (B.putStrLn $ "round trip parse with left overs" <> left)
    Fail -> B.putStrLn "uncaught parse error"

-- | run a dotParse erroring on leftovers, Fail or Err
runDotParser :: (DotParse a) => ByteString -> a
runDotParser = runParser_ dotParse

-- * Dot Grammar
-- | MergeEdges (strict)
--
data Strict = MergeEdges | NoMergeEdges deriving (Eq, Show, Generic)

instance DotParse Strict
  where
    dotPrint MergeEdges = "strict"
    dotPrint NoMergeEdges = ""

    dotParse = token $ optioned $(keyword "strict") (const $ pure MergeEdges) (pure NoMergeEdges)

defStrict :: Last Strict -> Strict
defStrict (Last Nothing) = NoMergeEdges
defStrict (Last (Just x)) = x

-- | Directed (digraph | graph)
data Directed = Directed | UnDirected deriving (Eq, Show, Generic)

instance DotParse Directed
  where
    dotPrint Directed = "digraph"
    dotPrint UnDirected = "graph"

    dotParse = token $
      (Directed <$ $(keyword "digraph")) <|>
      (UnDirected <$ $(keyword "graph"))

defDirected :: Last Directed -> Directed
defDirected (Last Nothing) = Directed
defDirected (Last (Just x)) = x

-- |
--
-- >>> runDotParser "0" :: ID
-- IDInt 0
--
-- >>> runDotParser "-.123" :: ID
-- IDDouble (-0.123)
--
-- >>> runParser dotParse "apple_1'" :: Result Error ID
-- OK (ID "apple_1") "'"
--
-- >>> :set -XQuasiQuotes
-- >>> runParser dotParse $ encodeUtf8 [trimming|"hello/""|] :: Result Error ID
-- OK (IDQuoted "hello\"") ""
--
-- >>> runDotParser (encodeUtf8 [trimming|<The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>|]) :: ID
-- IDHtml "<The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>"
data ID = ID ByteString | IDInt Int | IDDouble Double | IDQuoted ByteString | IDHtml ByteString deriving (Eq, Show, Generic, Ord)

instance DotParse ID
  where
    dotPrint (ID s) = s
    dotPrint (IDInt i) = packUTF8 (show i)
    dotPrint (IDDouble x) = packUTF8 (show x)
    dotPrint (IDQuoted x) =
      wrapQuotePrint x
    dotPrint (IDHtml s) = s

    -- order matters
    dotParse =
      (ID <$> ident) <|>
      (IDInt <$> (signed int `notFollowedBy` $(char '.'))) <|>
      (IDDouble <$> signed double) <|>
      (IDQuoted . packUTF8 <$> quoted) <|>
      (IDHtml . packUTF8 <$> htmlLike)

label :: ID -> String
label (ID s) = unpackUTF8 s
label (IDInt i) = show i
label (IDDouble d) = show d
label (IDQuoted q) = unpackUTF8 q
label (IDHtml h) = unpackUTF8 h

-- | Attribute key-value pair
--
-- >>> runDotParser "shape=diamond" :: (ID,ID)
-- (ID "shape",ID "diamond")
--
-- >>> runDotParser "fontname=\"Arial\"" :: (ID,ID)
-- (ID "fontname",IDQuoted "Arial")
--
instance DotParse (ID,ID)
  where
    dotPrint (x0,x1) = dotPrint x0 <> "=" <> dotPrint x1

    dotParse = token $
      do
        x0 <- token dotParse
        _ <- token $(symbol "=")
        x1 <- dotParse
        pure (x0,x1)

-- | Attribute collections
--
-- >>> runDotParser "[shape=diamond; color=blue] [label=label]" :: Map.Map ID ID
-- fromList [(ID "color",ID "blue"),(ID "label",ID "label"),(ID "shape",ID "diamond")]
--
-- A given entity can have multiple attribute lists. For simplicity, these are mconcat'ed on parsing.
--
instance DotParse (Map.Map ID ID)
  where
    dotPrint as =
      bool
      (wrapSquarePrint (intercalate ";" $ dotPrint <$> Map.toList as))
      mempty
      (as == Map.empty)

    dotParse =
      Map.fromList . mconcat . fmap toList <$>
      token (many (wrapSquareP (nonEmptyP dotParse sepP)) <|> ([] <$ wrapSquareP ws))

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
newtype GlobalAttributeStatement = GlobalAttributeStatement { globalAttributeStatement :: (ID, ID) } deriving (Eq, Show, Generic)

instance DotParse GlobalAttributeStatement
  where
    dotPrint (GlobalAttributeStatement s) = dotPrint s
    dotParse = GlobalAttributeStatement <$> dotParse

data AttributeType = GraphType | NodeType | EdgeType deriving (Eq, Show, Generic)

instance DotParse AttributeType
  where
    dotPrint GraphType = "graph"
    dotPrint NodeType = "node"
    dotPrint EdgeType = "edge"

    dotParse = token
      (GraphType <$ $(keyword "graph")) <|>
      (NodeType <$ $(keyword "node")) <|>
      (EdgeType <$ $(keyword "edge"))

-- |
data AttributeStatement = AttributeStatement { attributeType :: AttributeType, globals :: Map.Map ID ID } deriving (Eq, Show, Generic)

instance DotParse AttributeStatement
  where
    dotPrint (AttributeStatement t as) =
      intercalate " "
      [dotPrint t, dotPrint as]

    dotParse = AttributeStatement <$> dotParse <*> dotParse

-- |
-- >>> runDotParser "A [shape=diamond; color=blue]" :: Statement
-- StatementNode (NodeStatement {nodeID = ID "A", port = Nothing, nodeAttrs = fromList [(ID "color",ID "blue"),(ID "shape",ID "diamond")]})
data NodeStatement = NodeStatement { nodeID :: ID, port :: Maybe Port, nodeAttrs :: Map.Map ID ID } deriving (Eq, Show, Generic)

instance DotParse NodeStatement
  where
    dotPrint (NodeStatement i p as) =
      intercalate " " $
      [dotPrint i] <>
      (dotPrint <$> maybeToList p) <>
      [dotPrint as]

    dotParse = NodeStatement <$> dotParse <*> optional dotParse <*> dotParse

-- |
data EdgeID =
  EdgeID ID (Maybe Port) |
  AnonymousEdge SubGraphStatement
  deriving (Eq, Show, Generic)

instance DotParse EdgeID
  where
    dotPrint (EdgeID e p) =
      mconcat $ [dotPrint e] <> (dotPrint <$> maybeToList p)
    dotPrint (AnonymousEdge s) = dotPrint s

    dotParse =
      (EdgeID <$> dotParse <*> optional dotParse) <|>
      (AnonymousEdge <$> dotParse)

-- | An edgeop is -> in directed graphs and -- in undirected graphs.
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

fromDirected :: Directed -> EdgeOp
fromDirected Directed = EdgeDirected
fromDirected UnDirected = EdgeUndirected

-- |
-- >>> runDotParser "A -> B [style=dashed, color=grey]" :: EdgeStatement
-- EdgeStatement {edgeOp = EdgeDirected, leftEdge = EdgeID (ID "A") Nothing, rightEdges = EdgeID (ID "B") Nothing :| [], edgeAttrs = fromList [(ID "color",ID "grey"),(ID "style",ID "dashed")]}
--
data EdgeStatement = EdgeStatement { edgeOp :: EdgeOp, leftEdge :: EdgeID, rightEdges :: NonEmpty EdgeID, edgeAttrs :: Map.Map ID ID } deriving (Eq, Show, Generic)

instance DotParse EdgeStatement
  where
    dotPrint (EdgeStatement l rs xs as) =
      intercalate " "
      ([intercalate (" " <> dotPrint l <> " ") (dotPrint <$> (rs:toList xs))] <>
       [dotPrint as])

    dotParse = token $ do
      l <- dotParse
      o0 <- dotParse
      r0 <- dotParse
      ors <- many ((,) <$> dotParse <*> dotParse)
      as <- dotParse
      bool
        (pure (EdgeStatement o0 l (r0:| (snd <$> ors)) as))
        empty
        (any ((/= o0) . fst) ors)

edgeID :: EdgeID -> Maybe ID
edgeID (EdgeID i _) = Just i
edgeID (AnonymousEdge (SubGraphStatement i _)) = i

edgeIDsNamed :: EdgeStatement -> [(ID, ID)]
edgeIDsNamed e = [(x,y) | (Just x, Just y) <- edgeIDs e]

edgeIDs :: EdgeStatement -> [(Maybe ID,Maybe ID)]
edgeIDs e = zip (id0:id1) id1
  where
    id0 = edgeID (view #leftEdge e)
    id1 = edgeID <$> toList (view #rightEdges e)

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

data Statement = StatementNode NodeStatement | StatementEdge EdgeStatement | StatementGlobalAttribute GlobalAttributeStatement | StatementAttribute AttributeStatement | StatementSubGraph SubGraphStatement deriving (Eq, Show, Generic)

instance DotParse Statement
  where
    dotPrint (StatementNode x) = dotPrint x
    dotPrint (StatementEdge x) = dotPrint x
    dotPrint (StatementAttribute x) = dotPrint x
    dotPrint (StatementGlobalAttribute x) = dotPrint x
    dotPrint (StatementSubGraph x) = dotPrint x

    dotParse = token (
      -- Order is important
      (StatementEdge <$> dotParse) <|>
      (StatementAttribute <$> dotParse) <|>
      (StatementNode <$> dotParse) <|>
      (StatementSubGraph <$> dotParse) <|>
      (StatementGlobalAttribute <$> dotParse))

-- | Representation of a full graphviz graph, as per the dot language specification
data Graph =
  Graph {
    strict :: Last Strict,
    directed :: Last Directed,
    graphid :: Last ID,
    nodeAttributes :: Map.Map ID ID,
    graphAttributes :: Map.Map ID ID,
    edgeAttributes :: Map.Map ID ID,
    globalAttributes :: Map.Map ID ID,
    nodes :: [NodeStatement],
    edges :: [EdgeStatement],
    subgraphs :: [SubGraphStatement]
  } deriving (Eq, Show, Generic)

instance Semigroup Graph
  where
    (Graph m d i na ga ea gs ns es ss) <> (Graph m' d' i' na' ga' ea' gs' ns' es' ss') =
      Graph (m<>m') (d<>d') (i<>i') (na<>na') (ga<>ga') (ea<>ea') (gs<>gs') (ns<>ns') (es<>es') (ss<>ss')

instance Monoid Graph
  where
    mempty = Graph mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

gattL :: ID -> Lens' Graph (Maybe ID)
gattL k = #globalAttributes % at k

attL :: AttributeType -> ID -> Lens' Graph (Maybe ID)
attL a k = lens (lookupAtt_ a k) (alterAtt_ a k)

-- FIXME:
lookupAtt_ :: AttributeType -> ID -> Graph -> Maybe ID
lookupAtt_ GraphType k g = graphAttributes g ^. at k
lookupAtt_ NodeType k g = nodeAttributes g ^. at k
lookupAtt_ EdgeType k g = edgeAttributes g ^. at k

alterAtt_ :: AttributeType -> ID -> Graph -> Maybe ID -> Graph
alterAtt_ GraphType k g v = g & #graphAttributes %~ (at k .~ v)
alterAtt_ NodeType k g v = g & #nodeAttributes %~ (at k .~ v)
alterAtt_ EdgeType k g v = g & #edgeAttributes %~ (at k .~ v)

outercalate :: ByteString -> [ByteString] -> ByteString
outercalate _ [] = mempty
outercalate a xs = a <> intercalate a xs <> a

instance DotParse Graph
  where
    dotPrint (Graph me d i na ga ea gs ns es ss) =
      intercalate " " $
      bool [] ["strict"] (me == Last (Just MergeEdges)) <>
      bool ["digraph"] ["graph"] (d == Last (Just UnDirected)) <>
      maybe [] ((:[]) . dotPrint) (getLast i) <>
      [wrapCurlyPrint $ outercalate "\n    "
       ( [dotPrint (AttributeStatement NodeType na)] <>
         [dotPrint (AttributeStatement GraphType ga)] <>
         [dotPrint (AttributeStatement EdgeType ea)] <>
         (dotPrint . GlobalAttributeStatement <$> Map.toList gs) <>
         (dotPrint <$> ns) <>
         (dotPrint <$> es) <>
         (dotPrint <$> ss))
      ]

    dotParse = token $ do
      me <- dotParse
      d <- dotParse
      i <- optional dotParse
      ss <- wrapCurlyP (many dotParse)
      let g =
            (mempty :: Graph) &
            #strict .~ Last (Just me) &
            #directed .~ Last (Just d) &
            #graphid .~ Last i
      pure $ addStatements ss g

addStatement :: Statement -> Graph -> Graph
addStatement (StatementNode n) g = g & #nodes %~ (<>[n])
addStatement (StatementEdge e) g = g & #edges %~ (<>[e])
addStatement (StatementSubGraph s) g = g & #subgraphs %~ (<>[s])
addStatement (StatementAttribute (AttributeStatement GraphType as)) g = g & #graphAttributes %~ (<> as)
addStatement (StatementAttribute (AttributeStatement NodeType as)) g = g & #nodeAttributes %~ (<> as)
addStatement (StatementAttribute (AttributeStatement EdgeType as)) g = g & #edgeAttributes %~ (<> as)
addStatement (StatementGlobalAttribute (GlobalAttributeStatement s)) g = g & #globalAttributes %~ (<> Map.fromList [s])

addStatements :: [Statement] -> Graph -> Graph
addStatements ss g = Prelude.foldr addStatement g ss

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

-- | run a dot string through graphviz, supplying arguments and collecting stdout
processDotWith :: Directed -> [String] -> ByteString -> IO ByteString
processDotWith d args i = do
  let cmd = case d of
        Directed -> "dot"
        UnDirected -> "neato"
  (r,input,e) <- readProcessWithExitCode cmd args i
  bool
    (error $ unpackUTF8 e)
    (pure input)
    (r==ExitSuccess)

-- | run a dot string through graphviz, collecting the augmented dot string output
processDot :: Directed -> ByteString -> IO ByteString
processDot d = processDotWith d ["-Tdot"]

-- | Augment a Graph via the graphviz process
processGraph :: Graph -> IO Graph
processGraph g =
  runDotParser <$> processDot (defDirected $ view #directed g) (dotPrint g)

-- * Bounding Box

--
instance DotParse (Rect Double)
  where
    dotPrint (Rect x z y w) =
      intercalate "," $
      packUTF8 . show <$> [x, y, z, w]

    dotParse = token rectP

bb :: Lens' Graph (Maybe (Rect Double))
bb = lens lookupbb_ alterbb_

alterbb_ :: Graph -> Maybe (Rect Double) -> Graph
alterbb_ g r =
  g &
  #graphAttributes %~ (at (ID "bb") .~ (IDQuoted . dotPrint <$> r))

lookupbb_ :: Graph -> Maybe (Rect Double)
lookupbb_ g = case runParser rectP . packUTF8 . label <$> view (attL GraphType (ID "bb")) g of
  Just (OK r _) -> Just r
  _ -> Nothing

-- | nodes lens
nodesPortL :: Lens' Graph (Map.Map ID (Maybe Port, Map.Map ID ID))
nodesPortL =
  lens (\g -> g & view #nodes &
         fmap (\x -> (view #nodeID x, (view #port x, view #nodeAttrs x))) & Map.fromList)
  (\g m -> g & #nodes .~ ((\(n,(p,a)) -> NodeStatement n p a) <$> Map.toList m))

-- | nodes lens ignoring/forgetting port information
nodesL :: Lens' Graph (Map.Map ID (Map.Map ID ID))
nodesL =
  lens (\g -> g & view #nodes &
         fmap (\x -> (view #nodeID x, view #nodeAttrs x)) & Map.fromList)
  (\g m -> g & #nodes .~ ((\(n,a) -> NodeStatement n Nothing a) <$> Map.toList m))

-- | nodes lens ignoring/forgetting port information
edgesL :: Lens' Graph (Map.Map (ID,ID) (Map.Map ID ID))
edgesL =
  lens edges' newEdges_

-- | edge & attribute map
-- ignores anonymous subgraphs
edges' :: Graph -> Map.Map (ID, ID) (Map.Map ID ID)
edges' g =
  Map.fromList $
  mconcat $ fmap (\(xs, a) -> (,a) <$> xs)
  [(edgeIDsNamed e, view #edgeAttrs e) | e <- view #edges g]

newEdges_ :: Graph -> Map.Map (ID, ID) (Map.Map ID ID) -> Graph
newEdges_ g m =
  g &
  #edges .~
  ((\((x0,x1),as) ->
      EdgeStatement
      (fromDirected (defDirected $ view #directed g))
      (EdgeID x0 Nothing)
      (EdgeID x1 Nothing:|[]) as) <$>
    Map.toList m)

-- | A specific attribute for all nodes in a grpah
nodesA :: Graph -> ID -> Map.Map ID (Maybe ID)
nodesA g a = fmap (Map.lookup a) (view nodesL g)

-- | edge attribute lookup
edgesA :: Graph -> ID -> Map.Map (ID,ID) (Maybe ID)
edgesA g a = fmap (Map.lookup a) (edges' g)

-- | node position (as a Point)
nodePos :: Graph -> Map.Map ID (Maybe (Point Double))
nodePos g =
  fmap
  (\case
      (Just (IDQuoted x')) -> Just (runParser_ pointP x')
      _ -> Nothing) $
  nodesA g (ID "pos")

nodeWidth :: Graph -> Map.Map ID (Maybe Double)
nodeWidth g =
  fmap (\case
           Just (IDDouble x') -> Just x'
           _ -> Nothing) $
  nodesA g (ID "width")

edgeWidth :: Graph -> Map.Map (ID, ID) (Maybe Double)
edgeWidth g =
  fmap (\case
           Just (IDDouble x') -> Just x'
           _ -> Nothing) $
  edgesA g (ID "width")

edgeSpline :: Graph -> Map.Map (ID, ID) (Maybe Spline)
edgeSpline g =
  fmap (\case
           Just (IDQuoted x') -> Just (runParser_ splineP x')
           _ -> Nothing) $
  edgesA g (ID "pos")

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
edgeInfo g w = [EdgeInfo (fromMaybe w (join w')) (splinePath p) | ((_, _), (Just p, w')) <- xs]
  where
    xs = Map.toList $
         merge
         (mapMissing (\_ v -> (v,Nothing)))
         dropMissing
         (zipWithMatched (\_ x y -> (x,Just y)))
         (edgeSpline g)
         (edgeWidth g)

-- |
--
-- https://graphviz.org/docs/attr-types/splineType/
-- format of the example is end point point and then triples (5,8,11 lengths are 1, 2 and 3 cubics)
--
splinePath :: Spline -> [PathData Double]
splinePath sp = s' <> p1' <> cs <> e'
  where
    s' = maybe [] (\s -> [StartP s, LineP $ view #splineP1 sp]) (view #splineStart sp)
    e' = maybe [] (\e -> [LineP e]) (view #splineEnd sp)
    p1' = [StartP (view #splineP1 sp)]
    cs = (\(x,y,z) -> CubicP x y z) <$> view #splineTriples sp

-- | convert a (processed) 'Graph' to a 'ChartSvg'
--
-- >>> import Chart
-- >>> import DotParse.Examples (exInt)
-- >>> writeChartSvg "exg1.svg" (graphToChart exInt)
--
graphToChart :: Graph -> ChartSvg
graphToChart g =
  mempty
    & #charts .~ unnamed (ps <> c0 <> [ts])
    & #svgOptions % #svgHeight .~ 500
    & #hudOptions .~ (mempty & #chartAspect .~ ChartAspect)
  where
    glyphs w = case view (attL NodeType (ID "shape")) g of
      Just (ID "circle") -> defaultGlyphStyle & #shape .~ CircleGlyph & #size .~ 72 * w & #borderSize .~ 0.5 & #borderColor .~ black & #color .~ transparent
      Just (ID "box") -> defaultGlyphStyle & #shape .~ RectSharpGlyph (h/w) & #size .~ 72 * w & #borderSize .~ 0.5 & #borderColor .~ black & #color .~ transparent
      -- defaults to circle
      _ -> defaultGlyphStyle & #shape .~ CircleGlyph & #size .~ 72 * w & #borderSize .~ 0.5 & #borderColor .~ black & #color .~ transparent
    h = maybe 0.5 (runParser_ double . packUTF8 . label) (view (attL NodeType (ID "height")) g)
    vshift' = -3.7
    -- node information
    ns = nodeInfo g 0.5
    -- edge information
    es = edgeInfo g 0.5
    -- paths
    ps = fmap (\(EdgeInfo w p) -> PathChart (defaultPathStyle & #borderSize .~ w & #borderColor .~ black & #color .~ transparent) p) es
    -- circles
    c0 = fmap (\(NodeInfo _ w p) -> GlyphChart (glyphs w) [p]) ns
    -- labels
    ts =
      TextChart (defaultTextStyle & #size .~ 14) ((\(NodeInfo l _ (Point x y)) -> (l,Point x (vshift' + y))) <$> ns)

toStatements :: Directed -> G.Graph ByteString -> [Statement]
toStatements d g =
  ((\x -> StatementNode $ NodeStatement (IDQuoted x) Nothing Map.empty) <$> G.vertexList g) <>
  ((\(x, y) ->
      StatementEdge $
      EdgeStatement
      (fromDirected d)
      (EdgeID (IDQuoted x) Nothing)
      (fromList [EdgeID (IDQuoted y) Nothing])
      Map.empty) <$> G.edgeList g)
