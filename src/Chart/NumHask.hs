{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}

module Chart.NumHask where

import Chart
import Chart.GraphViz
import Control.Lens
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import qualified Data.Text as Text
import Data.Text.Lazy (fromStrict)
import GHC.OverloadedLabels
import NumHask.Prelude

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

toEdge :: Dependency -> (Class, Class, Maybe Family)
toEdge (Dependency to' from' wrapper) = (from', to', wrapper)

graphNH :: G.Gr Class (Maybe Family)
graphNH = mkGraph classesNH (toEdge <$> dependencies)

-- writeChartSvg "nh.svg" $ graphToChart g
layout :: ConfigNH -> G.Gr Class (Maybe Family) -> IO (G.Gr (G.AttributeNode Class) (G.AttributeEdge (Maybe Family)))
layout cfg =
  layoutGraph
    (paramsNH cfg)
    G.Dot

data ConfigNH = ConfigNH
  { nhWidth :: Double,
    magic :: Double,
    rheight :: Double,
    rcolor :: Colour,
    ecolor :: Colour,
    esize :: Double,
    tsize :: Double,
    tnudge :: Double,
    psize :: Double,
    pheight :: Double,
    pwidth :: Double,
    pwpad :: Double
  }
  deriving (Eq, Show, Generic)

defaultConfigNH :: ConfigNH
defaultConfigNH = ConfigNH 0.005 36.08 0.3 (setOpac 0.1 $ palette1 0) (palette1 1) 0.005 0.04 -2.0 1.0 0.25 0.06 0.3

-- | Example parameters for GraphViz.
paramsNH :: ConfigNH -> G.GraphvizParams G.Node Class (Maybe Family) Cluster Class
paramsNH cfg =
  G.defaultParams
    { G.globalAttributes =
        [ G.NodeAttrs
            [ G.Shape G.BoxShape
            ],
          G.GraphAttrs
            [ G.Overlap G.ScaleOverlaps,
              G.Splines G.SplineEdges,
              G.Size (G.GSize (cfg ^. #psize) Nothing True)
            ],
          G.EdgeAttrs [G.ArrowSize 0]
        ],
      G.isDirected = False,
      G.isDotCluster = const True,
      G.clusterID = G.Str . (fromStrict . pack . show),
      G.clusterBy = \(n, l) -> G.C (clusters Map.! l) (G.N (n, l)),
      G.fmtNode = \(_, l) ->
        [ G.Height (cfg ^. #pheight),
          G.Width
            ( (cfg ^. #pwpad) + (cfg ^. #pwidth)
                * fromIntegral (Text.length $ (pack . show) l)
            )
        ]
    }

-- | convert the numhask class graph to a chart
chartNH :: ConfigNH -> G.Gr (G.AttributeNode Class) (G.AttributeEdge e) -> ChartSvg
chartNH cfg gr =
  mempty
    & #chartList .~ cs <> c0 <> [ts]
    & #hudOptions .~ mempty
    & #svgOptions %~ ((#outerPad ?~ 0.02) . (#chartAspect .~ ChartAspect))
  where
    g = getGraph gr
    bs = mconcat $ (\(_, _, _, ps) -> ps) <$> snd g
    ns = Map.toList $ fst g
    cs = infosToChart (cfg ^. #nhWidth) (cfg ^. #ecolor) . singletonCubic <$> bs
    ws = getWidth . fst . snd <$> G.labNodes gr
    c0 =
      zipWith
        ( \w (Point x y) ->
            Chart
              (RectA (defaultRectStyle & #borderSize .~ (cfg ^. #esize) & #color .~ (cfg ^. #rcolor)))
              [R (-m * w + x) (m * w + x) (-m * h + y) (m * h + y)]
        )
        (fromMaybe one <$> ws)
        (snd <$> ns)
    ts =
      Chart
        ( TextA
            (defaultTextStyle & #size .~ cfg ^. #tsize)
            (pack . show . fst <$> ns)
        )
        (PointXY . (+ Point 0 (cfg ^. #tnudge)) . snd <$> ns)
    m = cfg ^. #magic
    h = cfg ^. #rheight

-- | make the damn chart already!
--
-- >>> makeChartNH defaultConfigNH
--
-- ![chart](nh.svg)
makeChartNH :: ConfigNH -> IO ()
makeChartNH c = do
  g <- layout c $ mkGraph classesNH (toEdge <$> dependencies)
  writeChartSvg "nh.svg" $ chartNH c g

-- | magma chart
--
-- >>> makeChartMagma defaultConfigNH
--
-- ![chart](nhmagma.svg)
makeChartMagma :: ConfigNH -> IO ()
makeChartMagma c = do
  g <- layout c $ mkGraph magmaClasses (toEdge <$> dependencies)
  writeChartSvg "nhmagma.svg" $
    chartNH c g & #chartList
      %~ ( \x ->
             x
               <> [Chart (LineA $ defaultLineStyle & #color .~ Colour 0.9 0.2 0.02 1 & #width .~ 0.005 & #dasharray ?~ [0.03, 0.01] & #linecap ?~ LineCapRound) (PointXY <$> [Point 50 230, Point 370 230])]
         )
