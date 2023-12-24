{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Example of Dot graph construction for the <https://hackage.haskell.org/package/chart-svg chart-svg> class heirarchy.
module DotParse.Examples.AST where

import Algebra.Graph qualified as G
import Chart
-- import Data.Bifunctor
-- import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.String.Interpolate
import Data.Text (Text, pack)
import DotParse
import FlatParse.Basic
import GHC.Generics
import GHC.IO.Unsafe
import Optics.Core
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> :set -XOverloadedStrings


data Component = Component { name :: String, super :: String, lens :: String } deriving (Eq, Show, Ord, Generic)

-- | Names of the various classes used in numhask
components :: [Component]
components =
  [ Component "ChartOptions" "" ""
  , Component "MarkupOptions" "ChartOptions" "#markupOptions"
  , Component "HudOptions" "ChartOptions" "#hudOptions"
  , Component "ChartTree" "ChartOptions" "#chartTree"
  , Component "markupHeight" "MarkupOptions" "#markupHeight"
  , Component "ChartAspect" "MarkupOptions" "#chartAspect"
  , Component "CssOptions" "MarkupOptions" "#cssOptions"
  , Component "RenderStyle" "MarkupOptions" "#renderStyle"
  ]

-- | algebraic graph
graphAST :: G.Graph String
graphAST =
  G.edges ((\c -> (view #super c, view #name c)) <$> components)
    <> G.vertices (view #name <$> components)

-- | NumHask statements in a dot Graph with box shapes for the nodes.
dotGraphAST :: Directed -> Graph
dotGraphAST d =
  defaultGraph
    & #directed .~ Last (Just d)
    & addStatements (toStatements d (strToUtf8 <$> graphAST))
    & attL NodeType (ID "shape") .~ Just (ID "box")
    & gattL (ID "rankdir") .~ Just (IDQuoted "BT")

-- | 'dotGraphNH' after being positionally processed via 'processGraph'
dotGraphNH' :: Directed -> Graph
dotGraphNH' d = unsafePerformIO $ processGraph (dotGraphAST d)
{-# NOINLINE dotGraphNH' #-}

-- | Convert a node ID to a label for chart-svg charts
-- Doing this directly in dot doesn't quite work because the engines get the width of the link wrong.
toLink :: ID -> Text
toLink id_ = [i|<a href="https://hackage.haskell.org/package/chart-svg/docs/#{t}.html\#t:#{t}">#{t}</a>|]
  where
    t = pack (label id_)

-- | A chart-svg chart with label links
--
-- > writeChartOptions "other/nh.svg" (graphToChart toLink (dotGraphNH' Directed))
--
-- ![NumHask Example](other/nh.svg)
writeASTChart :: IO ()
writeASTChart = writeChartOptions "other/ast.svg" (graphToChartWith (defaultChartConfig & #labelf .~ toLink & #chartColor .~ over lightness' (* 0.5) (palette 2) & #chartBackgroundColor .~ set opac' 0.1 (palette 1)) (dotGraphAST Directed))
