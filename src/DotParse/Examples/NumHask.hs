{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Example of Dot graph construction for the NumHask class heirarchy.
module DotParse.Examples.NumHask where

import qualified Algebra.Graph as G
import Chart
import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Text (Text, pack)
import DotParse
import GHC.IO.Unsafe
import Optics.Core
import Prelude hiding (replicate)
import Data.String.Interpolate
import FlatParse.Basic

-- $setup
-- >>> import DotParse
-- >>> :set -XOverloadedStrings

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
  | Actions
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
  -- Measure
  | Basis
  | Direction
  | Epsilon
  deriving (Show, Eq, Ord)

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
    Dependency Actions Distributive (Just Actor),
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
    Dependency Basis Distributive Nothing,
    Dependency Direction Distributive Nothing,
    Dependency Epsilon Subtractive Nothing,
    Dependency Epsilon MeetSemiLattice Nothing,
    Dependency Integral Ring Nothing,
    Dependency Ratio Field Nothing
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
    Basis,
    Direction,
    Actions,
    Integral,
    Ratio
  ]

classesModule :: [(Class, Text)]
classesModule =
  [ (Additive, "NumHask-Algebra-Additive"),
    (Subtractive, "NumHask-Algebra-Additive"),
    (Multiplicative, "NumHask-Algebra-Multiplicative"),
    (Divisive, "NumHask-Algebra-Multiplicative"),
    (Distributive, "NumHask-Algebra-Distributive"),
    (Ring, "NumHask-Algebra-Ring"),
    (Field, "NumHask-Algebra-Field"),
    (ExpField, "NumHask-Algebra-Field"),
    (QuotientField, "NumHask-Algebra-Field"),
    (TrigField, "NumHask-Algebra-Field"),
    (Basis, "NumHask-Algebra-Metric"),
    (Direction, "NumHask-Algebra-Metric"),
    (Actions, "NumHask-Algebra-Action"),
    (UpperBoundedField, "NumHask-Algebra-Field"),
    (LowerBoundedField, "NumHask-Algebra-Field"),
    (Integral, "NumHask-Data-Integral"),
    (Ratio, "NumHask-Data-Rational")
  ]

dependenciesNH :: [Dependency] -> [Dependency]
dependenciesNH = filter (\(Dependency x0 x1 _) -> x0 `elem` classesNH && x1 `elem` classesNH)

-- | NumHask Classes as an algebraic graph
graphNHG :: G.Graph Class
graphNHG =
  G.edges ((\(Dependency x y _) -> (x, y)) <$> dependenciesNH dependencies)
    <> G.vertices classesNH

-- | NumHask statements in a dot Graph with box shapes for the nodes.
dotGraphNH :: Directed -> Graph
dotGraphNH d =
  defaultGraph
    & #directed .~ Last (Just d)
    & addStatements (toStatements d (strToUtf8 . show <$> graphNHG))
    & attL NodeType (ID "shape") .~ Just (ID "box")
    & gattL (ID "rankdir") .~ Just (IDQuoted "BT")

-- | 'dotGraphNH' after being positionally processed via 'processGraph'
dotGraphNH' :: Directed -> Graph
dotGraphNH' d = unsafePerformIO $ processGraph (dotGraphNH d)
{-# NOINLINE dotGraphNH' #-}

-- | Convert a node ID to a label for chart-svg charts
-- Doing this directly in dot doesn't quite work because the engines get the width of the link wrong.
toLink :: ID -> Text
toLink id_ =[i|<a href="https://hackage.haskell.org/package/numhask/docs/#{m}.html\#t:#{t}">#{t}</a>|]
  where
    t = pack (label id_)
    m = Map.fromList (first (pack . show) <$> classesModule) Map.! t

-- | A chart-svg chart with label links
--
-- > writeChartOptions "other/nh.svg" (graphToChart toLink (dotGraphNH' Directed))
--
-- ![NumHask Example](other/nh.svg)
writeNHChart :: IO ()
writeNHChart = writeChartOptions "other/nh.svg" (graphToChartWith (defaultChartConfig & #labelf .~ toLink & #chartColor .~ over lightness' (* 0.5) (palette1 2) & #chartBackgroundColor .~ set opac' 0.1 (palette1 1)) (dotGraphNH' Directed))
  -- writeChartOptions "other/nh.svg" (graphToChartWith (defaultChartConfig & #labelf .~ toLink) (dotGraphNH' Directed))
