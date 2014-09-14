{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | All binaries in PATH.

module Data.Conduit.Shell.PATH where

import Data.Conduit.Shell.TH

$(generateBinaries)
