-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Support
-- Copyright   :  (c) Mads Buch 2021
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  me@madsbuch.com
-- Stability   :  experimental
--
-- See clear in the fog
--
-----------------------------------------------------------------------------

module Control.Monad.Support where

import Control.Monad.Probability

-- Support Monad
class ProbabilityMonad m => SupportMonad m where
  support :: m a -> [a]