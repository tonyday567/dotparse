{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Parser & Printer for the dot language of graphviz.
-- http://www.graphviz.org/doc/info/lang.html
module DotParse
 ( module DotParse.Types,
   module DotParse.FlatParse,
   module FlatParse.Basic
 ) where

import DotParse.Types
import FlatParse.Basic hiding (cut, lines)
import DotParse.FlatParse
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> :set -XOverloadedStrings
