{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Example of Dot graph construction for the <https://hackage.haskell.org/package/base base> class heirarchy.
module DotParse.Examples.Base where

import Algebra.Graph qualified as G
import Chart
import Data.Bifunctor
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Maybe
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
  = Eq
  | Eq1
  | Eq2
  | Bits
  | FiniteBits
  | Ord
  | Ix
  | Ord1
  | Ord2
  | Real
  | Num
  | Fractional
  | RealFrac
  | Floating
  | RealFloat
  | Enum
  | Integral
  | Functor
  | Foldable
  | Applicative
  | Bifunctor
  | Traversable
  | Foldable1
  | Bifoldable
  | Alternative
  | Monad
  | Bitraversable
  | MonadPlus
  | MonadFix
  | MonadZip
  | MonadFail
  | MonadIO
  | Show
  | Show1
  | Show2
  | Read
  | Read1
  | Read2
  | Semigroup
  | Monoid
  | Category
  | Arrow
  | ArrowZero
  | ArrowChoice
  | ArrowApply
  | ArrowLoop
  | ArrowPlus
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
  [ Dependency Eq1 Eq,
    Dependency Eq2 Eq1,
    Dependency Bits Eq,
    Dependency FiniteBits Bits,
    Dependency Ord Eq,
    Dependency Ix Ord,
    Dependency Ord1 Ord,
    Dependency Ord2 Ord1,
    Dependency Real Ord,
    Dependency Real Num,
    Dependency Fractional Num,
    Dependency Integral Enum,
    Dependency Integral Real,
    Dependency RealFrac Real,
    Dependency Floating Fractional,
    Dependency RealFloat RealFrac,
    Dependency RealFloat Floating
  ]

dependenciesR :: [Dependency]
dependenciesR = (\(Dependency x y) -> Dependency y x) <$> dependencies

-- | List of classes to use in diagram.
classesEq :: [Class]
classesEq =
  [ Eq
  , Eq1
  , Eq2
  , Bits
  , FiniteBits
  , Ord
  , Ix
  , Ord1
  , Ord2
  , Real
  , Num
  , Fractional
  , RealFrac
  , Floating
  , RealFloat
  , Enum
  , Integral
  ]

-- | Names of the modules where each class is located.
classesModule :: [(Class, Text)]
classesModule =
  [ (Eq, "Data-Eq")
  , (Eq1, "Data-Functor-Classes")
  , (Eq2, "Data-Functor-Classes")
  , (Bits, "Data-Bits")
  , (FiniteBits, "Data-Bits")
  , (Ord, "Data-Ord")
  , (Ix, "Data-Ix")
  , (Ord1, "Data-Functor-Classes")
  , (Ord2, "Data-Functor-Classes")
  , (Real, "GHC-Real")
  , (Num, "GHC-Num")
  , (Fractional, "GHC-Real")
  , (RealFrac, "GHC-Real")
  , (Floating, "GHC-Float")
  , (RealFloat, "GHC-Float")
  , (Enum, "GHC-Enum")
  , (Integral, "GHC-Real")
  ]

-- | List of dependencies to draw.
dependenciesEq :: [Dependency] -> [Dependency]
dependenciesEq = filter (\(Dependency x0 x1) -> x0 `elem` classesEq && x1 `elem` classesEq)

-- | NumHask Classes as an algebraic graph
graphEqG :: G.Graph Class
graphEqG =
  G.edges ((\(Dependency x y) -> (x, y)) <$> dependenciesEq dependenciesR)
    <> G.vertices classesEq

-- | statements in a dot Graph with box shapes for the nodes.
dotGraphEq :: Directed -> Graph
dotGraphEq d =
  defaultGraph
    & #directed .~ Last (Just d)
    & addStatements (toStatements d (strToUtf8 . show <$> graphEqG))
    & attL NodeType (ID "shape") .~ Just (ID "box")
    & gattL (ID "rankdir") .~ Just (IDQuoted "BT")

-- | 'dotGraphNH' after being positionally processed via 'processGraph'
dotGraphEq' :: Directed -> Graph
dotGraphEq' d = unsafePerformIO $ processGraph (dotGraphEq d)
{-# NOINLINE dotGraphEq' #-}

-- | Convert a node ID to a label for chart-svg charts
-- Doing this directly in dot doesn't quite work because the engines get the width of the link wrong.
toLink :: ID -> Text
toLink id_ = [i|<a href="https://hackage.haskell.org/package/base/docs/#{m}.html\#t:#{t}">#{t}</a>|]
  where
    t = pack (label id_)
    m = fromMaybe (error (label id_)) $ Map.lookup t (Map.fromList (first (pack . show) <$> classesModule))

-- | A chart-svg chart with label links
--
-- > writeChartOptions "other/nh.svg" (graphToChart toLink (dotGraphNH' Directed))
--
-- ![NumHask Example](other/nh.svg)
writeEqChart :: IO ()
writeEqChart = writeChartOptions "other/eq.svg" (graphToChartWith (defaultChartConfig & #labelf .~ toLink & #chartColor .~ over lightness' (* 0.5) (palette1 2) & #chartBackgroundColor .~ set opac' 0.1 (palette1 1) & #textSize .~ 20 & #nodeHeight .~ 0.2 & #nodeSize .~ 0.2 & #vshift .~ (-3.7) & #chartScale .~ 20 & #chartHeight .~ 400) (dotGraphEq' Directed))
