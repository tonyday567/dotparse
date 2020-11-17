{-# LANGUAGE OverloadedLabels #-}

module Chart.NumHask where

import NumHask.Prelude
import Chart.GraphViz
import Chart
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Graph.Inductive as G
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Control.Lens
import qualified Data.List as List

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
  -- Higher-kinded numbers
  | AdditiveAction
  | SubtractiveAction
  | MultiplicativeAction
  | DivisiveAction
  | Module
  -- Lattice
  | JoinSemiLattice
  | MeetSemiLattice
  | Lattice
  | BoundedJoinSemiLattice
  | BoundedMeetSemiLattice
  | BoundedLattice
  -- Integrals
  | Integral
  -- Measure
  | Norm
  | Signed
  | Epsilon
  deriving (Show, Eq, Ord)

data Cluster
  = GroupCluster
  | LatticeCluster
  | FieldCluster
  | HigherKindedCluster
  | MeasureCluster
  | NumHaskCluster
  deriving (Show, Eq, Ord)

clusters :: Map.Map Class Cluster
clusters = Map.fromList
  [ (Magma, GroupCluster),
    (Unital,GroupCluster),
    (Associative,GroupCluster),
    (Commutative,GroupCluster),
    (Invertible,GroupCluster),
    (Idempotent,GroupCluster),
    (Absorbing,GroupCluster),
    (Group,GroupCluster),
    (AbelianGroup,GroupCluster),
    (Additive,NumHaskCluster),
    (Subtractive,NumHaskCluster),
    (Multiplicative,NumHaskCluster),
    (Divisive,NumHaskCluster),
    (Distributive,NumHaskCluster),
    (Semiring,NumHaskCluster),
    (Ring,NumHaskCluster),
    (IntegralDomain,NumHaskCluster),
    (Field,FieldCluster),
    (ExpField,FieldCluster),
    (QuotientField,FieldCluster),
    (UpperBoundedField,FieldCluster),
    (LowerBoundedField,FieldCluster),
    (TrigField,FieldCluster),
    (AdditiveAction,HigherKindedCluster),
    (SubtractiveAction,HigherKindedCluster),
    (MultiplicativeAction,HigherKindedCluster),
    (DivisiveAction,HigherKindedCluster),
    (Module,HigherKindedCluster),
    (JoinSemiLattice,LatticeCluster),
    (MeetSemiLattice,LatticeCluster),
    (Lattice,LatticeCluster),
    (BoundedJoinSemiLattice,LatticeCluster),
    (BoundedMeetSemiLattice,LatticeCluster),
    (BoundedLattice,LatticeCluster),
    (Norm,MeasureCluster),
    (Signed,MeasureCluster),
    (Epsilon,MeasureCluster)
  ]

data Family
  = Addition
  | Multiplication
  | Actor
  deriving (Show, Eq, Ord)

data Dependency = Dependency
  { _class :: Class
  , _dep :: Class
  , _op :: Maybe Family
  } deriving (Show, Eq, Ord)

dependencies :: [Dependency]
dependencies =
  [ Dependency Unital Magma Nothing
  , Dependency Associative Magma Nothing
  , Dependency Commutative Magma Nothing
  , Dependency Invertible Magma Nothing
  , Dependency Idempotent Magma Nothing
  , Dependency Absorbing Magma Nothing
  , Dependency Group Unital Nothing
  , Dependency Group Invertible Nothing
  , Dependency Group Associative Nothing
  , Dependency AbelianGroup Unital Nothing
  , Dependency AbelianGroup Invertible Nothing
  , Dependency AbelianGroup Associative Nothing
  , Dependency AbelianGroup Commutative Nothing
  , Dependency Additive Commutative (Just Addition)
  , Dependency Additive Unital (Just Addition)
  , Dependency Additive Associative (Just Addition)
  , Dependency Subtractive Invertible (Just Addition)
  , Dependency Subtractive Additive (Just Addition)
  , Dependency Multiplicative Unital (Just Multiplication)
  , Dependency Multiplicative Associative (Just Multiplication)
  , Dependency Multiplicative Commutative (Just Multiplication)
  , Dependency Divisive Invertible (Just Multiplication)
  , Dependency Divisive Multiplicative (Just Multiplication)
  , Dependency Distributive Additive (Just Addition)
  , Dependency Distributive Multiplicative (Just Multiplication)
  , Dependency Distributive Absorbing Nothing
  , Dependency Semiring Additive (Just Addition)
  , Dependency Semiring Distributive Nothing
  , Dependency Semiring Associative (Just Multiplication)
  , Dependency Semiring Unital (Just Multiplication)
  , Dependency Ring Distributive Nothing
  , Dependency Ring Subtractive (Just Addition)
  , Dependency IntegralDomain Ring Nothing
  , Dependency Field Ring Nothing
  , Dependency Field Divisive (Just Multiplication)
  , Dependency ExpField Field Nothing
  , Dependency QuotientField Field Nothing
  , Dependency QuotientField Ring Nothing
  , Dependency TrigField Field Nothing
  , Dependency UpperBoundedField Field Nothing
  , Dependency LowerBoundedField Field Nothing
  -- higher-kinded relationships
  , Dependency AdditiveAction Additive (Just Actor)
  , Dependency SubtractiveAction Subtractive (Just Actor)
  , Dependency MultiplicativeAction Multiplicative (Just Actor)
  , Dependency DivisiveAction Divisive (Just Actor)
  , Dependency Module Distributive (Just Actor)
  , Dependency Module MultiplicativeAction Nothing
  -- Lattice
  , Dependency JoinSemiLattice Associative Nothing
  , Dependency JoinSemiLattice Commutative Nothing
  , Dependency JoinSemiLattice Idempotent Nothing
  , Dependency MeetSemiLattice Associative Nothing
  , Dependency MeetSemiLattice Commutative Nothing
  , Dependency MeetSemiLattice Idempotent Nothing
  , Dependency Lattice JoinSemiLattice Nothing
  , Dependency Lattice MeetSemiLattice Nothing
  , Dependency BoundedJoinSemiLattice JoinSemiLattice Nothing
  , Dependency BoundedJoinSemiLattice Unital Nothing
  , Dependency BoundedMeetSemiLattice MeetSemiLattice Nothing
  , Dependency BoundedMeetSemiLattice Unital Nothing
  , Dependency BoundedLattice BoundedJoinSemiLattice Nothing
  , Dependency BoundedLattice BoundedMeetSemiLattice Nothing
  , Dependency Signed Multiplicative Nothing
  , Dependency Norm Additive Nothing
  , Dependency Epsilon Additive Nothing
  , Dependency Epsilon Subtractive Nothing
  , Dependency Epsilon MeetSemiLattice Nothing
  , Dependency Integral Distributive Nothing
  ]

fieldClasses :: [Class]
fieldClasses =
  [ Magma
  , Unital
  , Associative
  , Commutative
  , Invertible
  , Absorbing
  , Additive
  , Subtractive
  , Multiplicative
  , Divisive
  , Distributive
  , Ring
  , Field
  ]

allClasses :: [Class]
allClasses =
  [ Additive
  , Subtractive
  , Multiplicative
  , Divisive
  , Distributive
  , Ring
  , Field
  , ExpField
  , QuotientField
  , TrigField
  , Norm
  , Signed
  , Epsilon
  ]

toEdge :: Dependency -> (Class, Class, Maybe Family)
toEdge (Dependency to' from' wrapper) = ((from'), (to'), wrapper)

toNode :: Class -> Text
toNode = show

tAll :: G.Gr Class (Maybe Family)
tAll = mkGraph (allClasses) (toEdge <$> dependencies)

-- writeChartSvg "nh.svg" $ graphToChart g
layout :: G.Gr Class (Maybe Family) -> IO (G.Gr (G.AttributeNode Class) (G.AttributeEdge (Maybe Family)))
layout g =
  layoutGraph
  paramsNH
  G.Dot
  g

-- | Example parameters for GraphViz.
paramsNH :: G.GraphvizParams G.Node Class (Maybe Family) Cluster Class
paramsNH
  = G.defaultParams
    { G.globalAttributes =
      [ G.NodeAttrs
        [ G.Shape G.BoxShape
        ]
      , G.GraphAttrs
        [ G.Overlap G.KeepOverlaps,
          G.Splines G.SplineEdges,
          G.Size (G.GSize 1 Nothing True)
        ]
      , G.EdgeAttrs [G.ArrowSize 0]
      ],
      G.isDirected = True,
      G.isDotCluster = const False,
      G.fmtNode = \(_,l) -> [G.Height 0.25, G.Width (0.2 + 0.04 * fromIntegral $ Text.length $ show l)],
      G.clusterBy = \(n,l) -> G.C (clusters Map.! l) (G.N (n,l))
    }

-- | convert the example graph to a chart
--
-- >>> gg <- layoutGraph (defaultDiaParams :: GraphvizParams G.Node Int () () Int) Dot t1
-- >>> writeChartSvg "example.svg" (graphToChart gg)
--
nhChart :: (G.Gr (G.AttributeNode Class) (G.AttributeEdge e)) -> ChartSvg
nhChart gr =
  mempty &
  #chartList .~ cs <> c0 <> [ts] &
  #hudOptions .~ defaultHudOptions &
  #svgOptions %~ ((#outerPad .~ Just 0.4) . (#chartAspect .~ UnadjustedAspect))
  where
    g = getGraph gr
    bs = mconcat $ (\(_,_,_,ps) -> ps) <$> (snd g)
    ns = Map.toList $ fst g
    cs = infosToChart 1 (palette1 List.!! 0) . singletonInfo <$> bs
    ws = getWidth . fst . snd <$> G.labNodes gr
    c0 =
      zipWith
      (\w (Point x y) ->
         Chart (RectA (defaultRectStyle & #borderSize .~ 1 & #color %~ setOpac 0.1))
         [R (-magic*w + x) (magic*w + x) (-magic*h + y) (magic*h + y)])
      (fromMaybe one <$> ws) (snd <$> ns)
    ts = Chart (TextA (defaultTextStyle & #size .~ 12) (show . fst <$> ns)) (PointXY . (+Point 0 (-2)) . snd <$> ns)
    magic = 36.08
    h = 0.25


-- | convert the example graph to a chart
--
-- >>> gg <- layoutGraph (defaultDiaParams :: GraphvizParams G.Node Int () () Int) Dot t1
-- >>> writeChartSvg "example.svg" (graphToChart gg)
--
nhChart' :: (G.Gr (G.AttributeNode Class) (G.AttributeEdge e)) -> ChartSvg
nhChart' gr =
  mempty &
  #chartList .~ cs <> c0 <> [ts] &
  #hudOptions .~ mempty &
  #svgOptions %~ ((#outerPad .~ Just 0.02) . (#chartAspect .~ ChartAspect))
  where
    g = getGraph gr
    bs = mconcat $ (\(_,_,_,ps) -> ps) <$> (snd g)
    ns = Map.toList $ fst g
    cs = infosToChart 0.004 (palette1 List.!! 4) . singletonInfo <$> bs
    ws = getWidth . fst . snd <$> G.labNodes gr
    c0 =
      zipWith
      (\w (Point x y) ->
         Chart (RectA (defaultRectStyle & #borderSize .~ 0.005 & #color %~ setOpac 0.1))
         [R (-magic*w + x) (magic*w + x) (-magic*h + y) (magic*h + y)])
      (fromMaybe one <$> ws) (snd <$> ns)
    ts = Chart (TextA (defaultTextStyle & #size .~ 0.03) (show . fst <$> ns)) (PointXY . (+Point 0 (-2)) . snd <$> ns)
    magic = 36.08
    h = 0.3

nh' :: IO ()
nh' = do
  g <- layout $ mkGraph (allClasses) (toEdge <$> dependencies)
  writeChartSvg "nh.svg" $ nhChart' g
