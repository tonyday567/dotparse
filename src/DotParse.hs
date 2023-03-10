{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Parser & Printer for the dot language of <http://www.graphviz.org/doc/info/lang.html graphviz>
--
-- See "DotParse.Examples" for usage.
module DotParse
  ( module DotParse.Types,
    module DotParse.FlatParse,
    module FlatParse.Basic,
    module NeatInterpolation,
  )
where

import DotParse.FlatParse
import DotParse.Types
import FlatParse.Basic hiding (cut)
import NeatInterpolation
import Prelude hiding (replicate)
