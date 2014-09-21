{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- | All binaries in PATH.

module Data.Conduit.Shell.PATH where

import Control.Monad
import Data.Conduit.Shell.Process
import Data.Conduit.Shell.TH
import Data.List

$(generateBinaries)
