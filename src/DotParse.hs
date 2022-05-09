{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser & Printer for the dot language of graphviz.
-- http://www.graphviz.org/doc/info/lang.html
module DotParse
  ( module DotParse.Types,
    module DotParse.FlatParse,
    module FlatParse.Basic,
    module NeatInterpolation,
  )
where

import DotParse.FlatParse
import DotParse.Types
import FlatParse.Basic hiding (cut, lines)
import NeatInterpolation
import Prelude hiding (replicate)
