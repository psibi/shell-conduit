{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC
  -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- | All binaries in PATH.
module Data.Conduit.Shell.PATH where

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit.Shell.Process
import Data.Conduit.Shell.TH
import Data.Conduit.Shell.Variadic
import Data.List
import qualified Data.Text as T (unpack)
import Prelude
import System.Directory

-- | Helpful CD command.
cd
  :: (MonadIO m, CmdArg arg)
  => arg -> m ()
cd fp =
  case (toTextArg fp) of
    [] -> return ()
    (path:_) -> liftIO $ setCurrentDirectory (T.unpack path)

$(generateBinaries)
