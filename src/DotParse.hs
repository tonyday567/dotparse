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

module DotParse where

import FlatParse.Basic hiding (cut, lines)
import DotParse.FlatParse.TH hiding (merge)
import DotParse.FlatParse
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
import Data.Text (Text)
import Chart
import Data.Maybe
import Data.Map.Merge.Strict
import qualified Data.Text as Text

-- $setup
-- >>> import DotParse
-- >>> import DotParse.FlatParse
-- >>> import DotParse.FlatParse.TH
-- >>> import FlatParse.Basic
-- >>> import Data.Proxy
-- >>> import NeatInterpolation
-- >>> import Data.Text.Encoding (encodeUtf8)
-- >>> :set -XOverloadedStrings

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

-- * printing
class DotParse a where
  dotPrint :: a -> ByteString
  dotParse :: Parser Error a

-- * Dot Grammar


-- | MergeEdges (strict)
--
data MergeEdges = MergeEdges | NoMergeEdges deriving (Eq, Show, Generic)

instance DotParse MergeEdges
  where
    dotPrint MergeEdges = "strict"
    dotPrint NoMergeEdges = ""

    dotParse = token $ optioned $(keyword "strict") (const $ pure MergeEdges) (pure NoMergeEdges)

-- | Directed (digraph | graph)
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
-- >>> runParser dotParse "apple_1'" :: Result Error ID
-- OK (IDString "apple_1") "'"
--
-- FIXME:
-- >>> :set -XQuasiQuotes
-- >>> runParser dotParse $ encodeUtf8 [trimming|"quoted \""|] :: Result Error ID
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

processGraph :: Graph -> IO Graph
processGraph g =
  runDotParser <$> dotB (directed g) (dotPrint g)

-- |
bb :: Graph -> Maybe (Rect Double)
bb g = case runParser rectP . packUTF8 <$> v of
  Just (OK r _) -> Just r
  _ -> Nothing
  where
    v = case Map.lookup (IDString "bb") (attributes g) of
      (Just (IDQuoted q)) -> Just q
      _ -> Nothing

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

toStatementsShow :: (Show a, Ord a) => G.Graph a -> [Statement]
toStatementsShow g =
  ((\x -> StatementNode $ NodeStatement (NodeID (IDQuoted (show x)) Nothing) (AttrList [])) <$> G.vertexList g) <>
  ((\(x, y) ->
      StatementEdge $
      EdgeStatement
      (EdgeID (Left (NodeID (IDQuoted (show x)) Nothing)))
      (EdgeRHSs $ fromList [EdgeRHS EdgeDirected (EdgeID (Left (NodeID (IDQuoted (show y)) Nothing)))])
      (AttrList [])) <$> G.edgeList g)
