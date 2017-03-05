{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Variadic process calling.
module Data.Conduit.Shell.Variadic
  ( ProcessType(..)
  , variadicProcess
  , CmdArg(..)
  ) where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Conduit.Shell.Process
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Control.Applicative (pure)

-- | A variadic process maker.
variadicProcess
  :: (ProcessType r)
  => String -> r
variadicProcess name = spr name []

-- | Make the final conduit.
makeProcessLauncher :: String -> [ST.Text] -> Segment ()
makeProcessLauncher name args = proc name (map ST.unpack args)

-- | Process return type.
class ProcessType t  where
  spr :: String -> [ST.Text] -> t

instance (r ~ ()) =>
         ProcessType (Segment r) where
  spr name args = makeProcessLauncher name args

-- | Accept strings as arguments.
instance (ProcessType r, CmdArg a) =>
         ProcessType (a -> r) where
  spr name args = \a -> spr name (args ++ toTextArg a)

-- | Command line argument.
class CmdArg a  where
  toTextArg :: a -> [ST.Text]

instance CmdArg ST.Text where
  toTextArg = pure . id

instance CmdArg LT.Text where
  toTextArg = pure . LT.toStrict

instance CmdArg SB.ByteString where
  toTextArg = pure . ST.decodeUtf8

instance CmdArg LB.ByteString where
  toTextArg = pure . LT.toStrict . LT.decodeUtf8

instance CmdArg String where
  toTextArg = pure . ST.pack

instance CmdArg [String] where
  toTextArg = map ST.pack

instance CmdArg [ST.Text] where
  toTextArg = map id

instance CmdArg [LT.Text] where
  toTextArg = map LT.toStrict

instance CmdArg [SB.ByteString] where
  toTextArg = map ST.decodeUtf8

instance CmdArg [LB.ByteString] where
  toTextArg = map (LT.toStrict . LT.decodeUtf8)
