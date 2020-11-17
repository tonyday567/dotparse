{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chart.GraphViz where

import NumHask.Prelude
import Chart
import Data.GraphViz hiding (dotAttributes)
import qualified Data.Graph.Inductive.Graph  as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map.Strict as Map
import qualified Data.GraphViz.Attributes.Complete as G
import Data.GraphViz.Commands.IO (hGetDot)
import Data.GraphViz.Types.Generalised (FromGeneralisedDot (..))
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Control.Lens

-- | example graph
example1 :: Gr Int ()
example1 = mkGraph [0..19]
        (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
         ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
         ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
        )

-- | Construct a graph from a list of vertex labels (which must be unique) and
--   a list of (directed) edges.  The result is suitable as input to 'layoutGraph'.
mkGraph :: Ord v => [v] -> [(v,v,e)] -> Gr v e
mkGraph vs es =
  G.mkGraph nodes edges
  where
    nodes = zip [0..] . toList . Set.fromList $ vs
    nmap   = Map.fromList $ swap <$>  nodes
    edges  = catMaybes $ mkEdge <$> es
    -- discards invalid edges
    mkEdge (v1,v2,e) =
      (,,) <$> Map.lookup v1 nmap <*> Map.lookup v2 nmap <*> pure e

-- | Example parameters for GraphViz.
defaultParamsChart :: GraphvizParams G.Node v e cl v
defaultParamsChart
  = defaultParams
    { globalAttributes =
      [ NodeAttrs
        [ G.Shape Circle,
          G.Height 0.5
        ]
      , GraphAttrs
        [ G.Overlap G.KeepOverlaps,
          G.Splines G.SplineEdges,
          G.Size (G.GSize 1 Nothing True)
        ]
      , EdgeAttrs [G.ArrowSize 0]
      ],
      isDirected = True
    }

-- | Round-trip a graph through an external graphviz layout algorithm, and read back in a version annotated with explicit positioning information.
layoutGraph
  :: (Ord cl, G.Graph gr)
  => GraphvizParams G.Node v e cl l
  -> GraphvizCommand
  -> gr v e
  -> IO (gr (AttributeNode v) (AttributeEdge e))
layoutGraph params com gr = dotAttributes com (isDirected params) gr' asDot
  where
    asDot = graphToDot params' gr'
    params' = params { fmtEdge = setEdgeIDAttribute $ fmtEdge params }
    gr' = addEdgeIDs gr

-- This should not be exported.  It is more or less copied from the
-- graphviz package source; the problem is that graphviz does not
-- export any way to have this parameterized by the GraphvizCommand.
dotAttributes :: (G.Graph gr, PPDotRepr dg G.Node, FromGeneralisedDot dg G.Node)
                  => GraphvizCommand -> Bool -> gr v (EdgeID e)
                  -> dg G.Node -> IO (gr (AttributeNode v) (AttributeEdge e))
dotAttributes command _isDir gr asDot
  = augmentGraph gr . parseDG <$> graphvizWithHandle command asDot DotOutput hGetDot
  where
    parseDG = (`asTypeOf` asDot) . fromGeneralised


-- | Decompose an annotated, concretely laid-out graph into a map from vertex labels to
--   points and a collection of edges associating vertex and edge
--   labels to 'Path' values.
getGraph
  :: Ord v
  => Gr (AttributeNode v) (AttributeEdge e)
  -> (Map.Map v (Point Double), [(v, v, e, [CubicPosition Double])])
getGraph gr = (vmap, edges)
  where
    nodes = G.labNodes gr
    vmap = Map.fromList
      [ (v, gpoint pt) | (_,(attrs,v)) <- nodes, G.Pos (G.PointPos pt) <- attrs ]
    ixmap = Map.fromList [ (i,v) | (i,(_,v)) <- nodes ]
    edges = [ (ixmap Map.! i, ixmap Map.! j, e, getPath attrs)
            | (i, j, (attrs,e)) <- G.labEdges gr
            ]
    getPath attrs = case [ss | G.Pos (G.SplinePos ss) <- attrs] of
      [splines] -> mconcat . map getSpline $ splines
      _ -> mempty
    getSpline (G.Spline { G.splinePoints = pt1:pts}) = fixedBeziers
      where
        ptGroups = F.toList $ F.toList <$> (Seq.chunksOf 3 $ Seq.fromList (fmap gpoint pts))
        fixedBeziers = zipWith mkBez (gpoint pt1 : fmap List.last ptGroups) ptGroups
        mkBez x1 [c1,c2,x2] = CubicPosition x1 x2 c1 c2
        mkBez _ _ = CubicPosition zero zero zero zero
    getSpline _ = []

-- | convert from a graphviz point to a chart point.
gpoint :: G.Point -> Point Double
gpoint (G.Point {G.xCoord = x, G.yCoord = y}) = Point x y

-- | get width from attribute list
--
-- >>> g <- layoutGraph (defaultDiaParams :: GraphvizParams G.Node Int () () Int) Dot t1
-- >>> getWidth . fst . snd <$> G.labNodes g
getWidth :: Attributes -> Maybe Double
getWidth xs = case [w |(G.Width w) <- xs] of
  [] -> Nothing
  (x:_) -> Just x

-- | convert a cubic position information to path info suitable for a chart.
singletonInfo :: CubicPosition Double -> [(PathInfo Double, Point Double)]
singletonInfo pos = [(StartI, start), (CubicI control1 control2, end)]
  where
    (CubicPolar start end control1 control2) = cubicPolar pos

infosToChart :: Double -> Colour -> [(PathInfo Double, Point Double)] -> Chart Double
infosToChart w c ps =
  Chart
  (PathA $ defaultPathStyle &
   #color .~ transparent &
   #borderColor .~ c &
   #borderSize .~ w &
   #pathInfo .~ (fst <$> ps))
  (PointXY . snd <$> ps)

-- | make example chart
--
-- ![example](example.svg)
example :: IO ()
example = do
  gg <- layoutGraph (defaultParamsChart :: GraphvizParams G.Node Int () () Int)
    Dot example1
  writeChartSvg "example.svg" (graphToChart gg)

-- | convert the example graph to a chart
--
-- >>> gg <- layoutGraph (defaultDiaParams :: GraphvizParams G.Node Int () () Int) Dot t1
-- >>> writeChartSvg "example.svg" (graphToChart gg)
--
graphToChart :: (Gr (AttributeNode Int) (AttributeEdge ())) -> ChartSvg
graphToChart gr =
  mempty &
  #chartList .~ cs <> c0 <> [ts] &
  #hudOptions .~ mempty & -- defaultHudOptions &
  #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ UnadjustedAspect))
  where
    g = getGraph gr
    bs = mconcat $ (\(_,_,_,ps) -> ps) <$> (snd g)
    ns = (first show <$> (Map.toList $ fst g)) :: [(Text,Point Double)]
    cs = infosToChart 1 (Colour 0.3 0.88 0.5 0.6) . singletonInfo <$> bs
    ws = getWidth . fst . snd <$> G.labNodes gr
    c0 = zipWith (\w n -> Chart (GlyphA (defaultGlyphStyle & #shape .~ CircleGlyph & #size .~ w & #borderSize .~ 0)) [PointXY . snd $ n]) ((\x -> x/0.5*36.08) . fromMaybe 0.5 <$> ws) ns
    ts = Chart (TextA (defaultTextStyle & #size .~ 12) (fst <$> ns)) (PointXY . snd <$> ns)

