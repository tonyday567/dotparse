{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Example of Dot graph construction for the <https://hackage.haskell.org/package/numhask NumHask> class heirarchy.
module DotParse.Examples.NumHask where

import Algebra.Graph qualified as G
import Chart
import Data.Bifunctor
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.String.Interpolate
import Data.Text (Text, pack)
import DotParse
import FlatParse.Basic
import GHC.IO.Unsafe
import Optics.Core
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> :set -XOverloadedStrings

-- | Names of the various classes used in numhask
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
  | -- Measure
    Basis
  | Direction
  | Epsilon
  deriving (Show, Eq, Ord)

-- | A class dependency.
data Dependency = Dependency
  { _class :: Class,
    _dep :: Class
  }
  deriving (Show, Eq, Ord)

-- | List of all dependencies (as at v0.11)
dependencies :: [Dependency]
dependencies =
  [ Dependency Unital Magma,
    Dependency Associative Magma,
    Dependency Commutative Magma,
    Dependency Invertible Magma,
    Dependency Idempotent Magma,
    Dependency Absorbing Magma,
    Dependency Group Unital,
    Dependency Group Invertible,
    Dependency Group Associative,
    Dependency AbelianGroup Unital,
    Dependency AbelianGroup Invertible,
    Dependency AbelianGroup Associative,
    Dependency AbelianGroup Commutative,
    Dependency Additive Commutative,
    Dependency Additive Unital,
    Dependency Additive Associative,
    Dependency Subtractive Invertible,
    Dependency Subtractive Additive,
    Dependency Multiplicative Unital,
    Dependency Multiplicative Associative,
    Dependency Multiplicative Commutative,
    Dependency Divisive Invertible,
    Dependency Divisive Multiplicative,
    Dependency Distributive Additive,
    Dependency Distributive Multiplicative,
    Dependency Distributive Absorbing,
    Dependency Ring Distributive,
    Dependency Ring Subtractive,
    Dependency IntegralDomain Ring,
    Dependency Field Ring,
    Dependency Field Divisive,
    Dependency ExpField Field,
    Dependency QuotientField Field,
    Dependency QuotientField Ring,
    Dependency TrigField Field,
    Dependency UpperBoundedField Field,
    Dependency LowerBoundedField Field,
    -- higher-kinded relationships
    Dependency AdditiveAction Additive,
    Dependency SubtractiveAction Subtractive,
    Dependency MultiplicativeAction Multiplicative,
    Dependency DivisiveAction Divisive,
    Dependency Actions Distributive,
    -- Lattice
    Dependency JoinSemiLattice Associative,
    Dependency JoinSemiLattice Commutative,
    Dependency JoinSemiLattice Idempotent,
    Dependency MeetSemiLattice Associative,
    Dependency MeetSemiLattice Commutative,
    Dependency MeetSemiLattice Idempotent,
    Dependency Lattice JoinSemiLattice,
    Dependency Lattice MeetSemiLattice,
    Dependency BoundedJoinSemiLattice JoinSemiLattice,
    Dependency BoundedJoinSemiLattice Unital,
    Dependency BoundedMeetSemiLattice MeetSemiLattice,
    Dependency BoundedMeetSemiLattice Unital,
    Dependency BoundedLattice BoundedJoinSemiLattice,
    Dependency BoundedLattice BoundedMeetSemiLattice,
    Dependency Basis Distributive,
    Dependency Direction Distributive,
    Dependency Epsilon Subtractive,
    Dependency Epsilon MeetSemiLattice,
    Dependency Integral Ring,
    Dependency Ratio Field
  ]

-- | List of classes to use in diagram.
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

-- | Names of the modules where each class is located.
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

-- | List of dependencies to draw.
dependenciesNH :: [Dependency] -> [Dependency]
dependenciesNH = filter (\(Dependency x0 x1) -> x0 `elem` classesNH && x1 `elem` classesNH)

-- | NumHask Classes as an algebraic graph
graphNHG :: G.Graph Class
graphNHG =
  G.edges ((\(Dependency x y) -> (x, y)) <$> dependenciesNH dependencies)
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
toLink id_ = [i|<a href="https://hackage.haskell.org/package/numhask/docs/#{m}.html\#t:#{t}">#{t}</a>|]
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
