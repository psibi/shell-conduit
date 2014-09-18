{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Variadic process calling.

module Data.Conduit.Shell.Variadic
  (ProcessType(..)
  ,variadicProcess)
  where

import           Control.Monad.Trans.Resource
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Data.Conduit.Shell.Process
import           Data.Conduit.Shell.Types
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | A variadic process maker.
variadicProcess :: (ProcessType r)
                => String -> r
variadicProcess name = spr name []

-- | Make the final conduit.
makeProcessLauncher :: (MonadResource m)
                    => String -> [ST.Text] -> Conduit Chunk m Chunk
makeProcessLauncher name args = proc name (map ST.unpack args)

-- | Process return type.
class ProcessType t where
    spr :: String -> [ST.Text] -> t

-- | The real type should be:
--
-- @ConduitM Chunk Chunk m ()@
--
-- But with this more liberal instance head we catch all cases in the
-- instance resolver, and then apply the equality restrictions later.
--
instance (MonadResource m, c ~ Chunk, c' ~ Chunk, r ~ ()) => ProcessType (ConduitM c c' m r) where
    spr name args = makeProcessLauncher name (reverse args)

-- | Accept strings as arguments.
instance (ProcessType r,CmdArg a) => ProcessType (a -> r) where
    spr name args = \a -> spr name (toTextArg a : args)

-- | Command line argument.
class CmdArg a  where
  toTextArg :: a -> ST.Text

instance CmdArg ST.Text where
  toTextArg = id

instance CmdArg LT.Text where
  toTextArg = LT.toStrict

instance CmdArg SB.ByteString where
  toTextArg = ST.decodeUtf8

instance CmdArg LB.ByteString where
  toTextArg = LT.toStrict . LT.decodeUtf8

instance CmdArg String where
  toTextArg = ST.pack
