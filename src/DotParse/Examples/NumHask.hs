{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ?~" #-}

-- | Example of Dot graph construction for the NumHask class heirarchy.
module DotParse.Examples.NumHask where

import Prelude hiding (replicate)
import qualified Data.Map.Strict as Map
import Optics.Core
import GHC.IO.Unsafe
import Chart
import qualified Algebra.Graph.Labelled as L
import Data.Monoid
import DotParse
import qualified Algebra.Graph as G

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

dependenciesNH :: [Dependency] -> [Dependency]
dependenciesNH = filter (\(Dependency x0 x1 _) -> x0 `elem` classesNH && x1 `elem` classesNH)

graphNH :: L.Graph (First Family) Class
graphNH =
  L.edges ((\(Dependency x y l) -> (First l,x,y)) <$> dependenciesNH dependencies) <>
  L.vertices classesNH

graphNHG :: G.Graph Class
graphNHG =
  G.edges ((\(Dependency x y _) -> (x,y)) <$> dependenciesNH dependencies) <>
  G.vertices classesNH

fromFamily :: First Family -> Colour
fromFamily (First f) = case f of
  Nothing -> palette1 0
  Just Addition -> palette1 1
  Just Multiplication -> palette1 2
  Just Actor -> palette1 3

dotGraphNH :: Directed -> Graph
dotGraphNH d =
  defaultGraph &
  #directed .~ Last (Just d) &
  addStatements (toStatements d (packUTF8 . show <$> graphNHG)) &
  attL NodeType (ID "shape") .~ Just (ID "box") &
  gattL (ID "rankdir") .~ Just (IDQuoted "BT")

dotGraphNH' :: Directed -> Graph
dotGraphNH' d = unsafePerformIO $ processGraph (dotGraphNH d)
{-# NOINLINE dotGraphNH' #-}

-- |
--
-- > import Chart
-- > writeChartSvg "other/t1.svg" (graphToChart (dotGraphNH' UnDirected))
nhExample :: Directed -> ChartSvg
nhExample d = graphToChart (dotGraphNH' d)
